-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- Tests for Data.Rewriting.Pos

module Pos where

import Data.Rewriting.Pos

import Test.QuickCheck

propParallelTo :: Pos -> Pos -> Bool
propParallelTo = \p q -> parallelTo p q == parallelToRef p q

-- reference implementation
parallelToRef :: Pos -> Pos -> Bool
parallelToRef p q = not (p `above` q) && not (p `below` q)
