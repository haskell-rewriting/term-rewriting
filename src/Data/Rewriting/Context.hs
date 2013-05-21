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
