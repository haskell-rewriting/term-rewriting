module Data.Rewriting.Term.Props (
    vars,
    funs,
    isVar,
    isFun,
    isGround,
    isLinear,
) where

import Data.Rewriting.Term.Type
import qualified Data.MultiSet as MS

vars :: Term f v -> [v]
vars = foldTerm (\v -> [v]) (\f as -> concat as)

funs :: Term f v -> [f]
funs = foldTerm (\v -> []) (\f as -> f : concat as)

isVar :: Term f v -> Bool
isVar Var{} = True
isVar Fun{} = False

isFun :: Term f v -> Bool
isFun Var{} = False
isFun Fun{} = True

isGround :: Term f v -> Bool
isGround = null . vars

isLinear :: Ord v => Term f v -> Bool
isLinear = all (\(_, c) -> c == 1) . MS.toOccurList . MS.fromList . vars
