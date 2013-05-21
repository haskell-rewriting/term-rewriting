-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- Tests for Data.Rewriting.Rules

module Rule where

import Samples
import Arbitrary

import Data.Rewriting.Rule

import Test.HUnit

tests :: Test
tests = TestLabel "Rules Tests" $ TestList [
    TestCase $ assertEqual "isLeftLinear"
        True (isLeftLinear $ x --> f[x, x]),
    TestCase $ assertEqual "~isLeftLinear"
        False (isLeftLinear $ f[x,g[a,b,x],y] --> z),
    TestCase $ assertEqual "isCollapsing"
        True (isCollapsing $ g[a, b] --> x),
    TestCase $ assertEqual "~isCollapsing"
        False (isCollapsing $ z --> g[x,h[x]]),
    TestList []]

propLeftRightLinearDual :: Term' -> Term' -> Bool
propLeftRightLinearDual = dual isLeftLinear isRightLinear

propCollapsingExpandingDual :: Term' -> Term' -> Bool
propCollapsingExpandingDual = dual isCollapsing isExpanding

propErasingCreatingDual :: Term' -> Term' -> Bool
propErasingCreatingDual = dual isErasing isCreating

propLinear :: Term' -> Term' -> Bool
propLinear l r = isLinear (Rule l r) ==
    (isLeftLinear (Rule l r) && isRightLinear (Rule l r))

propValid :: Term' -> Term' -> Bool
propValid l r = isValid (Rule l r) ==
    not (isCreating (Rule l r) || isExpanding (Rule l r))

dual :: (Rule' -> Bool) -> (Rule' -> Bool) -> Term' -> Term' -> Bool
dual p q a b = p (Rule a b) == q (Rule b a)
