{-# OPTIONS_GHC -XFlexibleContexts#-}
module Data.Rewriting.Substitution.Parse (
  fromString,
  parse,
  parseIO
) where

import Prelude hiding (lex)
import qualified Data.Map as Map
import Data.Rewriting.Term.Type
import Data.Rewriting.Substitution.Type
import Data.Rewriting.Term.Parse (lex)
import qualified Data.Rewriting.Term.Parse as Term
import Control.Monad
import Text.Parsec hiding (parse)


parse :: (Ord v) =>
  Parsec String u f -> Parsec String u v -> Parsec String u (Subst f v)
parse funP varP = between lbrace rbrace $ liftM (fromMap . Map.fromList ) bindings
  where
    lbrace   = lex $ char '{'
    rbrace   = lex $ char '}'
    bindings = binding funP varP `sepBy` lex (char ',')
    termP    = Term.parse funP varP


binding :: Parsec String u f -> Parsec String u v -> Parsec String u (v, Term f v)
binding funP varP = liftM2 (,) varP (slash >> termP) <?> "binding"
  where
    slash = lex $ char '/'
    termP = Term.parse funP varP


fromString :: [String] -> String -> Either ParseError (Subst String String)
fromString xs = runP (parse funP (varP xs)) () ""
  where
    varP = Term.parseVar (Term.ident "(),{}/")
    funP = Term.parseFun (Term.ident "(),{}")


parseIO :: [String] -> String -> IO (Subst String String)
parseIO xs input = case fromString xs input of
  Left err -> do { putStr "parse error at "; print err; mzero }
  Right t  -> return t
