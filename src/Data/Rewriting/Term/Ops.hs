module Data.Rewriting.Term.Ops (
    withArity,
) where

import Data.Rewriting.Term.Type

withArity :: Term f v -> Term (f, Int) v
withArity = foldTerm Var (\f ts -> Fun (f, length ts) ts)
