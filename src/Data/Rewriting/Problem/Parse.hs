-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Christian Sternagel

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Rewriting.Problem.Parse (
  parseIO,
  parseFileIO,
  fromString,
  fromFile,
  fromCharStream,
  ProblemParseError (..)
  ) where

import Data.Rewriting.Utils.Parse (lex, par, ident)
import qualified Data.Rewriting.Problem.Type as Prob
import Data.Rewriting.Problem.Type (Problem)
import Data.Rewriting.Rule (Rule (..))
import qualified Data.Rewriting.Term as Term
import qualified Data.Rewriting.Rules as Rules

import Data.List (partition, union)
import Data.Maybe (isJust)
import Prelude hiding (lex, catch)
import Control.Exception (catch)
import Control.Monad.Error
import Control.Monad (liftM, liftM3)
import Text.Parsec hiding (parse)
import System.IO (readFile)

data ProblemParseError = UnknownParseError String
                       | UnsupportedStrategy String
                       | FileReadError IOError
                       | UnsupportedDeclaration String
                       | SomeParseError ParseError deriving (Show)

instance Error ProblemParseError where strMsg = UnknownParseError

parseFileIO :: FilePath -> IO (Problem String String)
parseFileIO file = do r <- fromFile file
                      case r of
                        Left err -> do { putStrLn "following error occured:"; print err; mzero }
                        Right t  -> return t

parseIO :: String -> IO (Problem String String)
parseIO string = case fromString string of
                    Left err -> do { putStrLn "following error occured:"; print err; mzero }
                    Right t  -> return t

fromFile :: FilePath -> IO (Either ProblemParseError (Problem String String))
fromFile file = fromFile' `catch` (return . Left . FileReadError) where
  fromFile' = fromCharStream sn `liftM` readFile file
  sn         = "<file " ++ file ++ ">"

fromString :: String -> Either ProblemParseError (Problem String String)
fromString = fromCharStream "supplied string"

fromCharStream :: (Stream s (Either ProblemParseError) Char)
                   => SourceName -> s -> Either ProblemParseError (Problem String String)
fromCharStream sourcename input =
  case runParserT parse initialState sourcename input of
    Right (Left e)  -> Left $ SomeParseError e
    Right (Right p) -> Right p
    Left e          -> Left e
  where initialState = Prob.Problem { Prob.startTerms = Prob.AllTerms ,
                                      Prob.strategy   = Prob.Full ,
                                      Prob.theory     = Nothing ,
                                      Prob.rules      = Prob.RulesPair { Prob.strictRules = [],
                                                                         Prob.weakRules = [] } ,
                                      Prob.variables  = [] ,
                                      Prob.symbols    = [] ,
                                      Prob.signature  = Nothing,
                                      Prob.comment    = Nothing }


type ParserState = Problem String String

type WSTParser s a = ParsecT s ParserState (Either ProblemParseError) a

modifyProblem :: (Problem String String -> Problem String String) -> WSTParser s ()
modifyProblem = modifyState

parsedVariables :: WSTParser s [String]
parsedVariables = Prob.variables `liftM` getState

parse :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Problem String String)
parse = spaces >> parseDecls >> eof >> getState where
  parseDecls = many1 parseDecl
  parseDecl =  decl "VAR"       vars       (\ e p -> p {Prob.variables = e `union` Prob.variables p})
           <|> decl "THEORY"    theory     (\ e p -> p {Prob.theory = maybeAppend Prob.theory e p})
           <|> decl "SIG"       signature  (\ e p -> p {Prob.signature = maybeAppend Prob.signature e p})
           <|> decl "RULES"     rules      (\ e p -> p {Prob.rules   = e, --FIXME multiple RULES blocks?
                                                        Prob.symbols = Rules.funsDL (Prob.allRules e) [] })
           <|> decl "STRATEGY"  strategy   (\ e p -> p {Prob.strategy = e})
           <|> decl "STARTTERM" startterms (\ e p -> p {Prob.startTerms = e})
           <|> (decl "COMMENT"   comment   (\ e p -> p {Prob.comment = maybeAppend Prob.comment e p}) <?> "comment")
           <|> (par comment >>= modifyProblem . (\ e p -> p {Prob.comment = maybeAppend Prob.comment e p}) <?> "comment")
  decl name p f = try (par $ do
      lex $ string name
      r <- p
      modifyProblem $ f r) <?> (name ++ " block")
  maybeAppend fld e p = Just $ maybe [] id (fld p) ++ e

vars :: (Stream s (Either ProblemParseError) Char) => WSTParser s [String]
vars = do vs <- many (lex $ ident "()," [])
          return vs

signature :: (Stream s (Either ProblemParseError) Char) => WSTParser s [(String,Int)]
signature = many fundecl
    where
        fundecl = par (do
            f  <- lex $ ident "()," []
            ar <- lex (read <$> many1 digit)
            return $ (f,ar))

theory :: (Stream s (Either ProblemParseError) Char) => WSTParser s [Prob.Theory String String]
theory = many thdecl where
    thdecl     = par ((equations >>= return . Prob.Equations)
              <|>     (idlist    >>= \ (x:xs) -> return $ Prob.SymbolProperty x xs))
    equations  = try (do
        vs <- parsedVariables
        lex $ string "EQUATIONS"
        many $ equation vs) <?> "EQUATIONS block"
    equation vs = do
        l <- Term.parseWST vs
        lex $ string "=="
        r <- Term.parseWST vs
        return $ Rule l r
    idlist      = many1 $ (lex $ ident "()," [])

rules :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Prob.RulesPair String String)
rules = do vs <- parsedVariables
           rs <- many $ rule vs
           let (s,w) = partition fst rs
           return Prob.RulesPair { Prob.strictRules = map snd s ,
                                   Prob.weakRules   = map snd w }
  where rule vs = do l <- Term.parseWST vs
                     sep <- lex $ (try $ string "->=") <|> string "->"
                     r <- Term.parseWST vs
                     return (sep == "->", Rule {lhs = l, rhs = r})

strategy :: (Stream s (Either ProblemParseError) Char) => WSTParser s Prob.Strategy
strategy = innermost <|> outermost where
  innermost = string "INNERMOST" >> return Prob.Innermost
  outermost = string "OUTERMOST" >> return Prob.Outermost

startterms :: (Stream s (Either ProblemParseError) Char) => WSTParser s Prob.StartTerms
startterms = basic <|> terms where
  basic = string "CONSTRUCTOR-BASED" >> return Prob.BasicTerms
  terms = string "FULL" >> return Prob.AllTerms

comment :: (Stream s (Either ProblemParseError) Char) => WSTParser s String
comment = withpars <|> liftM2 (++) idents comment <|> return ""
  where idents = many1 (noneOf "()")
        withpars = do _ <- char '('
                      pre <- comment
                      _ <- char ')'
                      suf <- comment
                      return $ "(" ++ pre ++ ")" ++ suf
