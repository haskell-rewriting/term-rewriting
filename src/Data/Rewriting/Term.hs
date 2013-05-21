-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Term (
    Term (..),
    -- * Important operations
    fold, map, vars, funs,
    -- * Reexported modules
    module Data.Rewriting.Term.Type,
    module Data.Rewriting.Term.Ops,
    module Data.Rewriting.Term.Pretty,
    module Data.Rewriting.Term.Parse
) where

import Prelude ()
import Data.Rewriting.Term.Type
import Data.Rewriting.Term.Ops
import Data.Rewriting.Term.Pretty
import Data.Rewriting.Term.Parse
