module Data.Rewriting.Substitution.Type (
    Substitution,
    Substitution',
    module Data.Rewriting.Term.Type,
) where

import Data.Rewriting.Term.Type
import qualified Data.Map as M

type Substitution f v = M.Map v (Term f v)

type Substitution' v f v' = M.Map v (Term f v')
