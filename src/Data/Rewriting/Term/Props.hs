module Data.Rewriting.Term.Props (
    funs,
    funsDL,
    vars,
    varsDL,
    isVar,
    isFun,
    isGround,
    isLinear,
) where

import Data.Rewriting.Term.Type as Term
import qualified Data.MultiSet as MS

-- | Return the list of all variables in the term, from left to right.
--
-- >>> vars (Fun 'f' [Var 3, Fun 'f' [Var 1, Var 2, Var 3]])
-- [3,1,2,3]
vars :: Term f v -> [v]
vars = flip varsDL []

varsDL :: Term f v -> [v] -> [v]
varsDL = Term.fold (:) (const $ foldr (.) id)

-- | Return the list of all function symbols in the term, from left to right.
--
-- >>> funs (Fun 'f' [Var 3, Fun 'g' [Fun 'f' []]])
-- "fgf"
funs :: Term f v -> [f]
funs = flip funsDL []

funsDL :: Term f v -> [f] -> [f]
funsDL = Term.fold (const id) (foldl (.) . (:))


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
