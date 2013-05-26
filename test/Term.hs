-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- Tests for Data.Rewriting.Term

module Term where

import Arbitrary

import Data.Rewriting.Pos
import Data.Rewriting.Term

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)

propReplaceAt1 :: Pos -> Term' -> Term' -> Bool
propReplaceAt1 p t t' = fromMaybe True $ do
    u <- replaceAt t p t'
    v <- u `subtermAt` p
    return $ t' == v

propReplaceAt2 :: Pos -> Term' -> Term' -> Term' -> Bool
propReplaceAt2 p t t1 t2 = fromMaybe True $ do
    u1 <- replaceAt t p t1
    u2 <- replaceAt t p t2
    return $ (u1 == u2) == (t1 == t2)
