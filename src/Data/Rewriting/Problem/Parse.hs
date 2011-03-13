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

import Data.List (partition)
import Data.Maybe (isJust)
import Prelude hiding (lex)
import Control.Monad.Error
import Control.Monad (liftM, liftM3)
import Text.Parsec hiding (parse)
import System.IO (readFile)

data ProblemParseError = UnknownParseError String 
                       | UnsupportedStrategy String
                       | FileReadError IOError
                       | VariablesBlockMissing
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
  where initialState = ( Nothing, 
                         Prob.Problem { Prob.startTerms = Prob.TermAlgebra , 
                                        Prob.strategy   = Prob.Full , 
                                        Prob.rules      = Prob.RulesPair { Prob.strictRules = [], 
                                                                           Prob.weakRules = [] } , 
                                        Prob.variables  = [] , 
                                        Prob.symbols    = [] , 
                                        Prob.comment    = Nothing })



type ParserState = (Maybe [String], Problem String String)

type WSTParser s a = ParsecT s ParserState (Either ProblemParseError) a

modifyProblem :: (Problem String String -> Problem String String) -> WSTParser s ()
modifyProblem f = modifyState f' where
  f' (vs, problem) = (vs, f problem)

setParsedVariables :: [String] -> WSTParser s ()
setParsedVariables vs = modifyState setvs where
  setvs (_, problem) = (Just vs, problem)
        
parsedVariables :: WSTParser s (Maybe [String])
parsedVariables = fst `liftM` getState

parse :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Problem String String)
parse = parseDecls >> eof >> (snd `liftM` getState) where 
  parseDecls = many1 (lex $ par parseDecl)
  parseDecl = decl     "VAR"       vars       (\ e p -> p {Prob.variables = e})
              <|> decl "RULES"     rules      (\ e p -> p {Prob.rules   = e, 
                                                          Prob.symbols = Rules.funsDL (Prob.allRules e) [] })
              <|> decl "STRATEGY"  strategy   (\ e p -> p {Prob.strategy = e})
              <|> decl "STARTTERM" startterms (\ e p -> p {Prob.startTerms = e})
              <|> decl "COMMENT"   comment    (\ e p -> p {Prob.comment = Just e})
              <|> (char '(' >> ident "()," [] >>= throwError . UnsupportedDeclaration)
              <?> "declaration block"
  decl name p f = do r <- (try (lex $ string name) >> p) <?> (name ++ " declaration")
                     modifyProblem $ f r


vars :: (Stream s (Either ProblemParseError) Char) => WSTParser s [String]
vars = do vs <- many (lex $ ident "()," [])
          setParsedVariables vs
          return vs
                
rules :: (Stream s (Either ProblemParseError) Char) => WSTParser s (Prob.RulesPair String String)
rules = do mvs <- parsedVariables
           case mvs of 
             Nothing -> throwError VariablesBlockMissing
             Just vs -> do rs <- many $ rule vs
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
  terms = string "FULL" >> return Prob.TermAlgebra            
  
comment :: (Stream s (Either ProblemParseError) Char) => WSTParser s String
comment = do pre <- idents
             par <- optionMaybe (withpars comment)
             suf <- if isJust par then idents else return ""
             return $ pre ++ maybe "" id par ++ suf 
  where idents = many (noneOf "()")
        withpars p = do _ <- char '('
                        s <- p
                        _ <- char ')'
                        return $ "(" ++ s ++ ")"
