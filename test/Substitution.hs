-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- Tests for Data.Rewriting.Substitution

module Substitution where

import Arbitrary

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Substitution
import Data.Rewriting.Substitution.Type

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Function
import qualified Data.Map as M

propCompose :: Subst' -> Subst' -> Term' -> Bool
propCompose s1 s2 t = (s1 `compose` s2) `apply` t == s1 `apply` (s2 `apply` t)

propUnify1 :: Term' -> Term' -> Bool
propUnify1 s t = Just False /= do
    (\u -> u `apply` s == u `apply` t) <$> unify s t

propUnify2 :: Term' -> Term' -> Bool
propUnify2 s t = Just False /= do
    equalSubst <$> unify s t <*> unifyRef s t

equalSubst :: (Ord v, Eq f) => Subst f v -> Subst f v -> Bool
equalSubst s1 s2 = ((==) `on` toMap) (id' `compose` s1) (id' `compose` s2)
  where
    id' = fromMap . M.fromList $
        [(v, Var v) | v <- M.keys (toMap s1) ++ M.keys (toMap s2)]
