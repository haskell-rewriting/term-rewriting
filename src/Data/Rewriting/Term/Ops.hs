module Data.Rewriting.Term.Ops (
    withArity,
) where

import Data.Rewriting.Term.Type

-- | Annotate each occurrence of a function symbol with its actual arity,
-- i.e., its number of arguments.
--
-- >>> withArity (Fun 'f' [Var 1, Fun 'f' []])
-- Fun ('f',2) [Var 1,Fun ('f',0) []]
withArity :: Term f v -> Term (f, Int) v
withArity = foldTerm Var (\f ts -> Fun (f, length ts) ts)
