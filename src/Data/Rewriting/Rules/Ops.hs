module Data.Rewriting.Rules.Ops (
    -- * Operations on Rules
    funs,                            
    funsDL,
    vars,
    varsDL,
    lhss, 
    rhss, 
    -- * Predicates on Rules
    isLinear, isLeftLinear, isRightLinear,
    isGround, isLeftGround, isRightGround,
    isErasing,
    isCreating,
    isDuplicating,
    isCollapsing,
    isValid,
) where

import Data.Rewriting.Rule (Rule)
import Data.Rewriting.Term (Term)
import qualified Data.Rewriting.Rule as Rule

lhss :: [Rule f v] -> [Term f v]
lhss = map Rule.lhs

rhss :: [Rule f v] -> [Term f v]
rhss = map Rule.rhs

funs :: [Rule f v] -> [f]
funs = flip funsDL []

funsDL :: [Rule f v] -> [f] -> [f]
funsDL rs fs = foldr Rule.funsDL fs rs

vars :: [Rule f v] -> [v]
vars = flip varsDL []

varsDL :: [Rule f v] -> [v] -> [v]
varsDL rs fs = foldr Rule.varsDL fs rs


isLinear :: Ord v => [Rule f v] -> Bool
isLinear = all Rule.isLinear

isLeftLinear :: Ord v => [Rule f v] -> Bool
isLeftLinear = all Rule.isLeftLinear

isRightLinear :: Ord v => [Rule f v] -> Bool
isRightLinear = all Rule.isRightLinear

isGround :: [Rule f v] -> Bool
isGround = all Rule.isGround

isLeftGround :: [Rule f v] -> Bool
isLeftGround = all Rule.isLeftGround

isRightGround :: [Rule f v] -> Bool
isRightGround = all Rule.isRightGround

isErasing :: Ord v => [Rule f v] -> Bool
isErasing = any Rule.isErasing

isCreating :: Ord v => [Rule f v] -> Bool
isCreating = any Rule.isCreating


isDuplicating :: Ord v => [Rule f v] -> Bool
isDuplicating = any Rule.isDuplicating

isCollapsing :: [Rule f v] -> Bool
isCollapsing = any Rule.isCollapsing

isExpanding :: [Rule f v] -> Bool
isExpanding = any Rule.isExpanding

isValid :: [Rule f v] -> Bool
isValid = all Rule.isExpanding
