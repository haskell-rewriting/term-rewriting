-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- Types and Arbitrary instances for tests.

module Arbitrary (
    Var',
    Fun',
    Term',
    Rule',
    Subst',
) where

import Data.Rewriting.Term (Term (..))
import Data.Rewriting.Rule (Rule (..))
import qualified Data.Rewriting.Rule as Rule
import Data.Rewriting.Substitution (Subst, GSubst)
import qualified Data.Rewriting.Substitution.Type as Subst

import Test.QuickCheck (Arbitrary (..), CoArbitrary (..), (><))
import Test.QuickCheck.Gen
import Control.Applicative
import Control.Monad
import qualified Data.Map as M

newtype Var' = Var' Char
    deriving (Eq, Ord)

newtype Fun' = Fun' Char
    deriving (Eq, Ord)

type Term' = Term Fun' Var'

type Rule' = Rule Fun' Var'

type Subst' = Subst Fun' Var'

instance Show Var' where
    showsPrec p (Var' c) = showsPrec p c

instance Show Fun' where
    showsPrec p (Fun' c) = showsPrec p c

instance Arbitrary Var' where
    arbitrary = Var' <$> growingElements "xyzuvw"

instance CoArbitrary Var' where
    coarbitrary (Var' c) = coarbitrary c

instance Arbitrary Fun' where
    arbitrary = Fun' <$> growingElements "fghijk"

instance CoArbitrary Fun' where
    coarbitrary (Fun' c) = coarbitrary c

constant :: Gen Fun'
constant = Fun' <$> growingElements "abcd"

instance Arbitrary Term' where
    arbitrary = oneof [
        Var <$> arbitrary,
        Fun <$> constant <*> pure [],
        Fun <$> arbitrary <*> args]
      where
        args = sized $ \n -> do
            k <- choose (1, 1 `max` n)
            let n' = if k == 1 then n else 2*n `div` k
            replicateM k (resize n' arbitrary)

instance CoArbitrary Term' where
    coarbitrary (Var x) = variant 0 . coarbitrary x
    coarbitrary (Fun f ts) = variant (-1) . (coarbitrary f >< coarbitrary ts)

instance Arbitrary Rule' where
    arbitrary = (Rule <$> arbitrary <*> arbitrary) `suchThat` Rule.isValid

instance CoArbitrary Rule' where
    coarbitrary (Rule l r) = coarbitrary l >< coarbitrary r

instance Arbitrary Subst' where
    arbitrary = Subst.fromMap . M.fromList <$> arbitrary

instance CoArbitrary Subst' where
    coarbitrary = coarbitrary . M.toList . Subst.toMap
