-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Author: Bertram Felgenhauer

module Data.Rewriting.Context (
    Ctxt,
    -- * Important operations
    ofTerm,
    apply,
    -- * Reexported modules
    module Data.Rewriting.Context.Type,
    module Data.Rewriting.Context.Ops,
) where

import Data.Rewriting.Context.Type
import Data.Rewriting.Context.Ops
