-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Christian Sternagel

module Data.Rewriting.Utils (
    dropCommonPrefix,
) where

-- | @dropCommonPrefix xs ys@ removes the common prefix of @xs@ and @ys@ and
-- returns the remaining lists as a pair.
--
-- >>>dropCommonPrefix [1,2,3] [1,2,4,1]
-- ([3], [4,1])
dropCommonPrefix :: Ord a => [a] -> [a] -> ([a], [a])
dropCommonPrefix (x:xs) (y:ys) | x == y = dropCommonPrefix xs ys
dropCommonPrefix   xs     ys            = (xs, ys)
