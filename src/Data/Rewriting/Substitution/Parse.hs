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
import Data.Rewriting.Term.Parse (lex, par)
import qualified Data.Rewriting.Term.Parse as Term
import Control.Monad
import Text.Parsec hiding (parse)


parse :: (Ord v) =>
    Parsec String u f -> Parsec String u v -> Parsec String u (Subst f v)
parse fun var = par $ liftM (fromMap . Map.fromList) bindings where
    bindings = binding fun var `sepBy` lex (char ',')


binding :: Parsec String u f -> Parsec String u v -> Parsec String u (v, Term f v)
binding fun var = liftM2 (,) var (slash >> term) <?> "binding" where
    slash = lex $ char '/'
    term  = Term.parse fun var


fromString :: [String] -> String -> Either ParseError (Subst String String)
fromString xs = runP (parse fun (var xs)) () "" where
    var = Term.parseVar (Term.ident "(),{}/")
    fun = Term.parseFun (Term.ident "(),{}")


parseIO :: [String] -> String -> IO (Subst String String)
parseIO xs input = case fromString xs input of
    Left err -> do { putStr "parse error at "; print err; mzero }
    Right t  -> return t
