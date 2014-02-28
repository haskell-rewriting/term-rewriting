-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Bertram Felgenhauer, Ilya Epifanov

module Data.Rewriting.Term.Ops (
    -- * Operations on Terms
    funs,
    funsDL,
    vars,
    varsDL,
    withArity,
    subtermAt,
    replaceAt,
    flatten,
    -- * Predicates on Terms
    isVar,
    isFun,
    isGround,
    isLinear,
    isInstanceOf,
    isVariantOf,
) where

import Data.Rewriting.Pos
import Data.Rewriting.Term.Type as Term
import Data.Rewriting.Substitution.Match
import Data.Maybe
import qualified Data.MultiSet as MS
import qualified Data.Rewriting.Utils.Either

import Control.Monad (guard)

-- | Annotate each occurrence of a function symbol with its actual arity,
-- i.e., its number of arguments.
--
-- >>> withArity (Fun 'f' [Var 1, Fun 'f' []])
-- Fun ('f',2) [Var 1,Fun ('f',0) []]
withArity :: Term f v -> Term (f, Int) v
withArity = Term.fold Var (\f ts -> Fun (f, length ts) ts)

-- | Return the subterm at a given position.
subtermAt :: Term f v -> Pos -> Maybe (Term f v)
subtermAt t [] = Just t
subtermAt (Fun _ ts) (p:ps) | p >= 0 && p < length ts = subtermAt (ts !! p) ps
subtermAt _ _ = Nothing

-- NOTE: replaceAt and Context.ofTerm have the same recusion structure; is
-- there a nice higher-order function to abstract from it?

-- | replace a subterm at a given position.
replaceAt :: Term f v -> Pos -> Term f v -> Maybe (Term f v)
replaceAt _ [] t' = Just t'
replaceAt (Fun f ts) (i:p) t' = do
    guard (i >= 0 && i < length ts)
    let (ts1, t:ts2) = splitAt i ts
    t <- replaceAt t p t'
    return $ Fun f (ts1 ++ t : ts2)
replaceAt _ _ _ = Nothing

-- | Return the list of all variables in the term, from left to right.
--
-- >>> vars (Fun 'g' [Var 3, Fun 'f' [Var 1, Var 2, Var 3]])
-- [3,1,2,3]
vars :: Term f v -> [v]
vars = flip varsDL []

-- | Difference List version of 'vars'.
-- We have @varsDL t vs = vars t ++ vs@.

varsDL :: Term f v -> [v] -> [v]
varsDL = Term.fold (:) (const $ foldr (.) id)

-- | Return the list of all function symbols in the term, from left to right.
--
-- >>> funs (Fun 'f' [Var 3, Fun 'g' [Fun 'f' []]])
-- "fgf"
funs :: Term f v -> [f]
funs = flip funsDL []

-- | Difference List version of 'funs'.
-- We have @funsDL t vs = funs t ++ vs@.

funsDL :: Term f v -> [f] -> [f]
funsDL = Term.fold (const id) (\f xs -> (f:) . foldr (.) id xs)

-- | Return 'True' if the term is a variable, 'False' otherwise.
isVar :: Term f v -> Bool
isVar Var{} = True
isVar Fun{} = False

-- | Return 'True' if the term is a function application, 'False' otherwise.
isFun :: Term f v -> Bool
isFun Var{} = False
isFun Fun{} = True

-- | Check whether the term is a ground term, i.e., contains no variables.
isGround :: Term f v -> Bool
isGround = null . vars

-- | Check whether the term is linear, i.e., contains each variable at most
-- once.
isLinear :: Ord v => Term f v -> Bool
isLinear = all (\(_, c) -> c == 1) . MS.toOccurList . MS.fromList . vars

-- | Check whether a term is an instance of another.
isInstanceOf :: (Eq f, Ord v, Ord v') => Term f v -> Term f v' -> Bool
isInstanceOf t u = isJust (match u t)

-- | Check whether a term is a variant of another.
isVariantOf :: (Eq f, Ord v, Ord v') => Term f v -> Term f v' -> Bool
isVariantOf t u = isInstanceOf t u && isInstanceOf u t

-- | Disambiguates the type of variable symbol after rewriting if
-- there's only one possible.
flatten :: Term f (Either v v) -> Term f v
flatten = Term.map id Data.Rewriting.Utils.Either.flatten
