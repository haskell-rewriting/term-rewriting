-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Ilya Epifanov

module Data.Rewriting.Utils.Either (
    flatten
) where


flatten :: Either v v -> v
flatten (Left v) = v
flatten (Right v) = v
