-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini

-- | Termination problem type, based on WST format.
module Data.Rewriting.Problem (
    Problem,
    -- * Reexported modules
    module Data.Rewriting.Problem.Type,
    module Data.Rewriting.Problem.Parse,
    module Data.Rewriting.Problem.Pretty,
) where

import Data.Rewriting.Problem.Type
import Data.Rewriting.Problem.Parse
import Data.Rewriting.Problem.Pretty
