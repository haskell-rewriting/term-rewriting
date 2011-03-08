module Data.Rewriting.Pos (
    Pos,
) where

-- | A position in a term. Arguments are counted from 1.
--
-- TODO: Should we used zero-based indexing? Which is less confusing?
type Pos = [Int]
