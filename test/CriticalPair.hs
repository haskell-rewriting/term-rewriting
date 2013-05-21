-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- Tests for Data.Rewriting.CriticalPair

module CriticalPair where

import Arbitrary
import Samples

import Data.Rewriting.Term (Term (..))
import qualified Data.Rewriting.Term as Term
import Data.Rewriting.Rule (Rule (..))
import Data.Rewriting.Pos (Pos)
import qualified Data.Rewriting.Substitution as Subst
import qualified Data.Rewriting.Rules as Rules
import qualified Data.Rewriting.Context as Ctxt
import Data.Rewriting.CriticalPair

import qualified Data.Set as S
import Test.HUnit

type CP' = CP Fun' Var' Var'

maxSize = 4

propValidCPs' :: [Rule'] -> Bool
propValidCPs' = all validCP . cps' . take maxSize

-- propValidCPs :: [Rule'] -> [Rule'] -> Bool
-- propValidCPs rs rs' = all validCP (cps (take maxSize rs) (take maxSize rs'))

propOuterCPs' :: [Rule'] -> Bool
propOuterCPs' = all (null . leftPos) . cpsOut' . take maxSize

propInnerCPs' :: [Rule'] -> Bool
propInnerCPs' = all (not . null . leftPos) . cpsIn' . take maxSize

tests :: Test
tests = TestLabel "Critical Pair Tests" $ TestList [
    TestCase $ assertEqual "CPs of fixed TRS"
        (cpSet cps1) (cpSet $ cps' trs1)
    ]

cpSet :: (Ord f, Ord v) => [CP f v v] -> S.Set (Term f (Either v v), Term f (Either v v), Term f (Either v v), Rule f v, Rule f v, Pos)
cpSet = S.fromList . map (\cp -> (top cp, left cp, right cp, leftRule cp, rightRule cp, leftPos cp))

validCP :: (Ord v, Eq f) => CP f v v -> Bool
validCP CP{ left = left, top = top, right = right, leftPos = pos,
            leftRule = lRule, rightRule = rRule, subst = subst } =
    subst `Subst.apply` Term.map id Right (lhs rRule) == top &&
    subst `Subst.apply` Term.map id Right (rhs rRule) == right &&
    subst `Subst.apply` Term.map id Left (lhs lRule) == top !!! pos &&
    subst `Subst.apply` Term.map id Left (rhs lRule) == left !!! pos &&
    Ctxt.ofTerm top pos == Ctxt.ofTerm left pos
