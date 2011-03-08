{-# LANGUAGE BangPatterns #-}
-- |
-- Simple rewriting.
--
-- Note: The rules are assumed to be non-creating, i.e., variables on the
-- rhs should also occur on the lhs. Rules violating this constraint
-- will have no effect.
module Data.Rewriting.Rules.Rewrite (
    Reduct,
    fullRewrite,
    outerRewrite,
    innerRewrite,
    -- * utilities not reexported from 'Data.Rewriting.Rules'
    headRewrite,
    nested,
    listContexts,
) where

import Data.Rewriting.Substitution
import Data.Rewriting.Pos
import Data.Rewriting.Rule

import Data.Maybe

-- | A reduct. It contains the resulting term, the position that the term
-- was rewritten at, and the applied rule.
--
-- TODO: should this be a proper record?
type Reduct f v v' = (Term f v, Pos, Rule f v')

-- | Full rewriting: Apply rules anywhere in the term.
--
-- Reducts are returned in pre-order: the first is a leftmost, outermost redex.
fullRewrite :: (Ord v', Ord v, Eq f)
    => [Rule f v'] -> Term f v -> [(Term f v, Pos, Rule f v')]
fullRewrite trs t = headRewrite trs t ++ nested (fullRewrite trs) t

-- | Outer rewriting: Apply rules at outermost redexes.
--
-- Reducts are returned in left to right order.
outerRewrite :: (Ord v', Ord v, Eq f)
    => [Rule f v'] -> Term f v -> [(Term f v, Pos, Rule f v')]
outerRewrite trs t = case headRewrite trs t of
    [] -> nested (outerRewrite trs) t
    rs -> rs

-- | Inner rewriting: Apply rules at innermost redexes.
--
-- Reducts are returned in left to right order.
innerRewrite :: (Ord v', Ord v, Eq f)
    => [Rule f v'] -> Term f v -> [(Term f v, Pos, Rule f v')]
innerRewrite trs t = case nested (innerRewrite trs) t of
    [] -> headRewrite trs t
    rs -> rs

-- | Head rewriting: Apply rules only at the root of the term.
--
-- This is useful as a building block for various rewriting strategies.
headRewrite :: (Ord v', Ord v, Eq f)
    => [Rule f v'] -> Term f v -> [(Term f v, Pos, Rule f v')]
headRewrite trs t = do
    r <- trs
    s <- maybeToList $ match (lhs r) t
    s' <- maybeToList $ gApply s (rhs r)
    return (s', [], r)

-- | Nested rewriting: Apply a rewriting strategy to all arguments of a
-- function symbol, left to right. For variables, the result will be empty.
--
-- This is another building block for rewriting strategies.
nested :: (Term f v -> [(Term f v, Pos, a)])
    -> Term f v -> [(Term f v, Pos, a)]
nested _ (Var _) = []
nested a (Fun f ts) = do
    (n, cl, t) <- listContexts ts
    (\(t', p, r) -> (Fun f (cl t'), n : p, r)) `fmap` a t

-- | Return a list of contexts of a list. Each returned element is an element
-- index (starting from 1), a function that replaces the list element by a
-- new one, and the original element.
listContexts :: [a] -> [(Int, a -> [a], a)]
listContexts = go 1 id where
    go !n f [] = []
    go !n f (x:xs) = (n, f . (: xs), x) : go (n+1) ((x:) . f) xs
