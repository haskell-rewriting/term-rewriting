-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Christian Sternagel

{-# LANGUAGE FlexibleContexts#-}
module Data.Rewriting.Substitution.Parse (
    fromString,
    parse,
    parseIO
) where

import Data.Rewriting.Utils.Parse (ident, lex, par)
import Prelude hiding (lex)
import qualified Data.Map as Map
import Data.Rewriting.Term.Type
import Data.Rewriting.Substitution.Type
import qualified Data.Rewriting.Term.Parse as Term
import Control.Monad
import Text.Parsec hiding (parse)
import Text.Parsec.Prim (runP)


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
    var = Term.parseVar $ ident "(),{}/" []
    fun = Term.parseFun $ ident "(),{}"  []


parseIO :: [String] -> String -> IO (Subst String String)
parseIO xs input = case fromString xs input of
    Left err -> do { putStr "parse error at "; print err; mzero }
    Right t  -> return t
