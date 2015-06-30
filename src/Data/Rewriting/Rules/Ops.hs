-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Bertram Felgenhauer

module Data.Rewriting.Rules.Ops (
    -- * Operations on Rules
    funs,
    funsDL,
    vars,
    varsDL,
    lhss,
    rhss,
    map,
    restrictFuns,
    -- * Predicates on Rules
    isLinear, isLeftLinear, isRightLinear,
    isGround, isLeftGround, isRightGround,
    isErasing,
    isCreating,
    isExpanding,
    isDuplicating,
    isCollapsing,
    isValid,
) where

import Prelude hiding (map)
import qualified Prelude as P
import Data.Rewriting.Rule (Rule)
import Data.Rewriting.Term (Term)
import qualified Data.Rewriting.Term as Term
import qualified Data.Rewriting.Rule as Rule


-- | @lhss rs@ returns the list of left-hand sides of @rs@
lhss :: [Rule f v] -> [Term f v]
lhss = P.map Rule.lhs

-- | @lhss rs@ returns the list of right-hand sides of @rs@
rhss :: [Rule f v] -> [Term f v]
rhss = P.map Rule.rhs

-- | Lifting of Term.'Term.funs' to list of rules.
funs :: [Rule f v] -> [f]
funs = flip funsDL []

-- | Difference List version of 'funs'.
-- We have @funsDL r vs = funs r ++ vs@.
funsDL :: [Rule f v] -> [f] -> [f]
funsDL rs fs = foldr Rule.funsDL fs rs

-- | Lifting of Term.'Term.vars' to list of rules.
vars :: [Rule f v] -> [v]
vars = flip varsDL []

-- | Lifting of Rule.'Rule.map' to list of rules.
map :: (f -> f') -> (v -> v') -> [Rule f v] -> [Rule f' v']
map f v = P.map (Rule.map f v)

-- | Difference List version of 'vars'.
-- We have @varsDL r vs = vars r ++ vs@.
varsDL :: [Rule f v] -> [v] -> [v]
varsDL rs fs = foldr Rule.varsDL fs rs

-- | Returns 'True' iff all given rules satisfy 'Rule.isLinear'
isLinear :: Ord v => [Rule f v] -> Bool
isLinear = all Rule.isLinear

-- | Returns 'True' iff all given rules satisfy 'Rule.isLeftLinear'
isLeftLinear :: Ord v => [Rule f v] -> Bool
isLeftLinear = all Rule.isLeftLinear

-- | Returns 'True' iff all given rules satisfy 'Rule.isRightLinear'
isRightLinear :: Ord v => [Rule f v] -> Bool
isRightLinear = all Rule.isRightLinear

-- | Returns 'True' iff all given rules satisfy 'Rule.isGroundLinear'
isGround :: [Rule f v] -> Bool
isGround = all Rule.isGround

-- | Returns 'True' iff all given rules satisfy 'Rule.isLeftGround'
isLeftGround :: [Rule f v] -> Bool
isLeftGround = all Rule.isLeftGround

-- | Returns 'True' iff all given rules satisfy 'Rule.isRightGround'
isRightGround :: [Rule f v] -> Bool
isRightGround = all Rule.isRightGround

-- | Returns 'True' iff any of the given rules satisfy 'Rule.isErasing'
isErasing :: Ord v => [Rule f v] -> Bool
isErasing = any Rule.isErasing

-- | Returns 'True' iff any of the given rules satisfy 'Rule.isCreating'
isCreating :: Ord v => [Rule f v] -> Bool
isCreating = any Rule.isCreating

-- | Returns 'True' iff any of the given rules satisfy 'Rule.isDuplicating'
isDuplicating :: Ord v => [Rule f v] -> Bool
isDuplicating = any Rule.isDuplicating

-- | Returns 'True' iff any of the given rules satisfy 'Rule.isCollapsing'
isCollapsing :: [Rule f v] -> Bool
isCollapsing = any Rule.isCollapsing

-- | Returns 'True' iff any of the given rules satisfy 'Rule.isExpanding'
isExpanding :: [Rule f v] -> Bool
isExpanding = any Rule.isExpanding

-- | Returns 'True' iff all rules satisfy 'Rule.isValid'
isValid :: Ord v => [Rule f v] -> Bool
isValid = all Rule.isValid

-- | Restrict the rules to those only using function symbols satisfying
-- the given predicate.
restrictFuns :: (f -> Bool) -> [Rule f v] -> [Rule f v]
restrictFuns funp = filter (all funp . Rule.funs)
