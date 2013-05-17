module Data.Rewriting.Substitution (
    GSubst,
    Subst,
    -- * Important operations
    gApply,
    apply,
    compose,
    -- * Reexported modules
    module Data.Rewriting.Substitution.Type,
    module Data.Rewriting.Substitution.Ops,
    module Data.Rewriting.Substitution.Match,
    module Data.Rewriting.Substitution.Unify,
    module Data.Rewriting.Substitution.Pretty,
    module Data.Rewriting.Substitution.Parse,
) where

import Data.Rewriting.Substitution.Type hiding (fromMap, toMap)
import Data.Rewriting.Substitution.Ops
import Data.Rewriting.Substitution.Match
import Data.Rewriting.Substitution.Unify
import Data.Rewriting.Substitution.Pretty
import Data.Rewriting.Substitution.Parse
