-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Rule.Ops (
    -- * Operations on Rules
    funs,
    funsDL,
    vars,
    varsDL,
    left,
    right,
    rename,
    -- * Predicates on Rules
    both,
    isLinear, isLeftLinear, isRightLinear,
    isGround, isLeftGround, isRightGround,
    isErasing,
    isCreating,
    isDuplicating,
    isCollapsing,
    isExpanding,
    isValid,
    isInstanceOf,
    isVariantOf,
) where

import Data.Rewriting.Rule.Type
import Data.Rewriting.Substitution (match, merge)
import qualified Data.Rewriting.Term as Term

import qualified Data.Set as S
import qualified Data.MultiSet as MS
import Data.Maybe

-- | Test whether the given predicate is true for both sides of a rule.
both :: (Term f v -> Bool) -> Rule f v -> Bool
both p r = p (lhs r) && p (rhs r)

-- | Apply a function to the lhs of a rule.
left :: (Term f v -> a) -> Rule f v -> a
left f = f . lhs

-- | Apply a function to the rhs of a rule.
right :: (Term f v -> a) -> Rule f v -> a
right f = f . rhs


-- | Lifting of 'Term.rename' to 'Rule': renames left- and right-hand sides.
-- 
-- >>> rename (+ 1) $ Rule {lhs = (Fun 'f' [Var 1, Fun 'g' [Var 2]]), rhs = Fun 'g' [Var 1]}
-- Rule {lhs = Fun 'f' [Var 2, Fun 'g' [Var 3]], rhs = Fun 'g' [Var 2]}
rename :: (v -> v') -> Rule f v -> Rule f v'
rename f rl = Rule (left (Term.rename f) rl) (right (Term.rename f) rl)


-- | Lifting of 'Term.funs' to 'Rule': returns the list of function symbols
-- in left- and right-hand sides.
--
-- >>> funs $ Rule {lhs = Fun 'f' [Var 3, Fun 'g' [Fun 'f' []]], rhs = Fun 'h' [Fun 'f' []]}
-- "fgfhf"
funs :: Rule f v -> [f]
funs = flip funsDL []

-- | Difference List version of 'funs'.
-- We have @funsDL r vs = funs r ++ vs@.
funsDL ::  Rule f v -> [f] -> [f]
funsDL r = Term.funsDL (lhs r) . Term.funsDL (rhs r)

-- | Lifting of 'Term.vars' to 'Rule': returns the list of variables in
-- left- and right-hand sides.
--
-- >>> vars $ Rule {lhs = Fun 'g' [Var 3, Fun 'f' [Var 1, Var 2, Var 3]], rhs = Fun 'g' [Var 4, Var 3]}
-- [3,1,2,3,4,3]
vars :: Rule f v -> [v]
vars = flip varsDL []

-- | Difference List version of 'vars'.
-- We have @varsDL r vs = vars r ++ vs@.
varsDL :: Rule f v -> [v] -> [v]
varsDL r = Term.varsDL (lhs r) . Term.varsDL (rhs r)

-- | Check whether both sides of the given rule are linear.
isLinear :: Ord v => Rule f v -> Bool
isLinear = both Term.isLinear

-- | Check whether the left hand side of the given rule is linear.
isLeftLinear :: Ord v => Rule f v -> Bool
isLeftLinear = left Term.isLinear

-- | Check whether the right hand side of the given rule is linear.
isRightLinear :: Ord v => Rule f v -> Bool
isRightLinear = right Term.isLinear

-- | Check whether both sides of the given rule is are ground terms.
isGround :: Rule f v -> Bool
isGround = both Term.isGround

-- | Check whether the left hand side of the given rule is a ground term.
isLeftGround :: Rule f v -> Bool
isLeftGround = left Term.isGround

-- | Check whether the right hand side of the given rule is a ground term.
isRightGround :: Rule f v -> Bool
isRightGround = right Term.isGround

-- auxiliary: return variables of term as Set
varsS :: Ord v => Term f v -> S.Set v
varsS = S.fromList . Term.vars

-- | Check whether the given rule is erasing, i.e., if some variable
-- occurs in the left hand side but not in the right hand side.
isErasing :: Ord v => Rule f v -> Bool
isErasing r = not $ varsS (lhs r) `S.isSubsetOf` varsS (rhs r)

-- | Check whether the given rule is creating, i.e., if some variable
-- occurs in its right hand side that does not occur in its left hand side.
--
-- This is the dual of 'isErasing'. The term /creating/ is non-standard.
-- Creating rules are usually forbidden. See also 'isValid'.
isCreating :: Ord v => Rule f v -> Bool
isCreating r = not $ varsS (rhs r) `S.isSubsetOf` varsS (lhs r)

-- auxiliary: return variables of term as MultiSet
varsMS :: Ord v => Term f v -> MS.MultiSet v
varsMS = MS.fromList . Term.vars

-- | Check whether the given rule is duplicating, i.e., if some variable
-- occurs more often in its right hand side than in its left hand side.
isDuplicating :: Ord v => Rule f v -> Bool
isDuplicating r = not $ varsMS (rhs r) `MS.isSubsetOf` varsMS (lhs r)

-- | Check whether the given rule is collapsing, i.e., if its right
-- hand side is a variable.
isCollapsing :: Rule f v -> Bool
isCollapsing = Term.isVar . rhs

-- | Check whether the given rule is expanding, i.e., if its left hand
-- sides is a variable.
--
-- This is the dual of 'isCollapsing'. The term /expanding/ is non-standard.
-- Expanding rules are usually forbidden. See also 'isValid'.
isExpanding :: Rule f v -> Bool
isExpanding = Term.isVar . lhs

-- | Check whether the given rule is non-creating and non-expanding.
-- See also 'isCreating' and 'isExpanding'
isValid :: Ord v => Rule f v -> Bool
isValid r = not (isCreating r) && not (isExpanding r)

-- | Check whether the first rule is an instance of the second rule.
isInstanceOf :: (Eq f, Ord v, Ord v') => Rule f v -> Rule f v' -> Bool
isInstanceOf r r' = case (match (lhs r') (lhs r), match (rhs r') (rhs r)) of
    (Just s, Just s') -> isJust (merge s s')
    _ -> False

-- | Check whether two rules are variants of each other.
isVariantOf :: (Eq f, Ord v, Ord v') => Rule f v -> Rule f v' -> Bool
isVariantOf t u = isInstanceOf t u && isInstanceOf u t
