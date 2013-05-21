-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini

module Data.Rewriting.Rule (
    Rule (..),
    -- * Reexported modules
    module Data.Rewriting.Rule.Type,
    module Data.Rewriting.Rule.Ops,
    module Data.Rewriting.Rule.Pretty,
) where

import Data.Rewriting.Rule.Type
import Data.Rewriting.Rule.Ops
import Data.Rewriting.Rule.Pretty
