-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Christian Sternagel, Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Pos (
    Pos,
    -- * Comparing Positions
    -- | Note that positions are not totally ordered. Nevertheless there are
    -- some commonly useful comparisons between positions.
    above,
    below,
    parallelTo,
    leftOf,
    rightOf,
) where

import Data.Rewriting.Utils
import Data.List

-- | A position in a term. Arguments are counted from 0.
--
-- A position describes a path in the tree representation of a term. The empty
-- position @[]@ denotes the root of the term. A position @[0,1]@ denotes the
-- 2nd child of the 1st child of the root (counting children from left to
-- right).
type Pos = [Int]

-- | @p \`above\` q@ checks whether @p@ is above @q@ (in the tree representation of
-- a term). A position @p@ is above a position @q@, whenever @p@ is a prefix of
-- @q@.
above :: Pos -> Pos -> Bool
above = isPrefixOf

-- | @p \`below\` q@ checks whether @p@ is below @q@, that is to say that @q@ is
-- above @p@.
below :: Pos -> Pos -> Bool
below = flip above

-- | @p \`parallelTo\` q@ checks whether @p@ is parallel to @q@, that is to say
-- that @p@ and @q@ do not lie on the same path.
parallelTo :: Pos -> Pos -> Bool
parallelTo p q = not (null p') && not (null q') where
    (p', q') = dropCommonPrefix p q

-- | @p \`leftOf\` q@ checks whether @p@ is left of @q@. This is only possible if
-- @p@ and @q@ do not lie on the same path (i.e., are parallel to each other).
leftOf :: Pos -> Pos -> Bool
leftOf p q = not (null p') && not (null q') && head p' < head q' where
    (p', q') = dropCommonPrefix p q

-- | @p \`rightOf\` q@ checks whether @p@ is right of @q@.
rightOf :: Pos -> Pos -> Bool
rightOf p q = not (null p') && not (null q') && head p' > head q' where
    (p', q') = dropCommonPrefix p q

-- reference implementations
parallelToRef :: Pos -> Pos -> Bool
parallelToRef p q = not (p `above` q) && not (p `below` q)
