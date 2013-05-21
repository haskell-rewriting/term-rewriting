-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

{-# LANGUAGE BangPatterns #-}
-- |
-- Simple rewriting.
--
-- Note: The rules are assumed to be non-creating, i.e., variables on the
-- rhs should also occur on the lhs. Rules violating this constraint
-- will have no effect.
module Data.Rewriting.Rules.Rewrite (
    Reduct (..),
    Strategy,
    fullRewrite,
    outerRewrite,
    innerRewrite,
    rootRewrite,
    -- * utilities not reexported from "Data.Rewriting.Rules"
    nested,
    listContexts,
) where

import Data.Rewriting.Substitution
import Data.Rewriting.Pos
import Data.Rewriting.Rule

import Data.Maybe

-- | A reduct. It contains the resulting term, the position that the term
-- was rewritten at, and the applied rule.
data Reduct f v v' = Reduct {
     result :: Term f v,
     pos :: Pos,
     rule :: Rule f v',
     subst :: GSubst v' f v
}

-- | A rewrite strategy.
type Strategy f v v' = Term f v -> [Reduct f v v']

-- | Full rewriting: Apply rules anywhere in the term.
--
-- Reducts are returned in pre-order: the first is a leftmost, outermost redex.
fullRewrite :: (Ord v', Eq v, Eq f)
    => [Rule f v'] -> Strategy f v v'
fullRewrite trs t = rootRewrite trs t ++ nested (fullRewrite trs) t

-- | Outer rewriting: Apply rules at outermost redexes.
--
-- Reducts are returned in left to right order.
outerRewrite :: (Ord v', Eq v, Eq f)
    => [Rule f v'] -> Strategy f v v'
outerRewrite trs t = case rootRewrite trs t of
    [] -> nested (outerRewrite trs) t
    rs -> rs

-- | Inner rewriting: Apply rules at innermost redexes.
--
-- Reducts are returned in left to right order.
innerRewrite :: (Ord v', Eq v, Eq f)
    => [Rule f v'] -> Strategy f v v'
innerRewrite trs t = case nested (innerRewrite trs) t of
    [] -> rootRewrite trs t
    rs -> rs

-- | Root rewriting: Apply rules only at the root of the term.
--
-- This is mainly useful as a building block for various rewriting strategies.
rootRewrite :: (Ord v', Eq v, Eq f)
    => [Rule f v'] -> Strategy f v v'
rootRewrite trs t = do
    r <- trs
    s <- maybeToList $ match (lhs r) t
    t' <- maybeToList $ gApply s (rhs r)
    return Reduct{ result = t', pos = [], rule = r, subst = s }

-- | Nested rewriting: Apply a rewriting strategy to all arguments of a
-- function symbol, left to right. For variables, the result will be empty.
--
-- This is another building block for rewriting strategies.
nested :: Strategy f v v' -> Strategy f v v'
nested _ (Var _) = []
nested s (Fun f ts) = do
    (n, cl, t) <- listContexts ts
    (\r -> r{ result = Fun f (cl (result r)), pos = n : pos r }) `fmap` s t

-- | Return a list of contexts of a list. Each returned element is an element
-- index (starting from 0), a function that replaces the list element by a
-- new one, and the original element.
listContexts :: [a] -> [(Int, a -> [a], a)]
listContexts = go 0 id where
    go !n f [] = []
    go !n f (x:xs) = (n, f . (: xs), x) : go (n+1) (f . (x:)) xs
