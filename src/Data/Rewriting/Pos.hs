module Data.Rewriting.Pos (
    Pos,
    above,
    below,
    parallelTo,
    leftOf,
    rightOf,
) where

import Data.List

-- | A position in a term. Arguments are counted from 1.
--
-- TODO: Should we used zero-based indexing? Which is less confusing?
type Pos = [Int]

above :: Pos -> Pos -> Bool
above = isPrefixOf

below :: Pos -> Pos -> Bool
below = flip above

parallelTo :: Pos -> Pos -> Bool
parallelTo p q = not (p `above` q) && not (p `below` q)

leftOf :: Pos -> Pos -> Bool
leftOf p q = p `parallelTo` q && p <= q

rightOf :: Pos -> Pos -> Bool
rightOf p q = p `parallelTo` q && p >= q
