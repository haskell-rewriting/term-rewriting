-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Martin Avanzini, Ilya Epifanov

-- | Operations on lists of rules.
--
-- See also "Data.Rewriting.CriticalPair"
module Data.Rewriting.Rules (
    -- * Important operations
    fullRewrite,
    -- * Reexported modules
    module Data.Rewriting.Rules.Rewrite,
    module Data.Rewriting.Rules.Ops,
    module Data.Rewriting.Rules.Confluent
) where

import Data.Rewriting.Rules.Ops
import Data.Rewriting.Rules.Rewrite hiding (nested, listContexts)
import Data.Rewriting.Rules.Confluent
