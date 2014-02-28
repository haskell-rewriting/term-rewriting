-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Ilya Epifanov

module Data.Rewriting.Axiom.Ops (
  orient,
  fromRule
) where

import Data.Rewriting.Axiom.Type
import Data.Rewriting.Rule.Type

-- | Orients an axiom into a rule.
orient :: (Ord f, Ord v) => Axiom f v -> Rule f v
orient (Axiom t1 t2)
  | t1 >= t2 = Rule t1 t2
  | otherwise = Rule t2 t1

-- | Unorients a rule into an axiom
fromRule :: Rule f v -> Axiom f v
fromRule (Rule lhs rhs) = Axiom lhs rhs
