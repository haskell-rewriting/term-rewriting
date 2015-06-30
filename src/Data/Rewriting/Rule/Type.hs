-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

module Data.Rewriting.Rule.Type (
    module Data.Rewriting.Term.Type,
    Rule (..),
    map,
    mapSides
) where

import Prelude hiding (map)
import Data.Rewriting.Term.Type hiding (map, fold)
import qualified Data.Rewriting.Term.Type as T

-- | Rewrite rule with left-hand side and right-hand side.
data Rule f v = Rule { lhs :: Term f v, rhs :: Term f v }
    deriving (Ord, Eq, Show)


mapSides :: (Term f v -> Term f' v') -> Rule f v -> Rule f' v'
mapSides f r = Rule{ lhs = f (lhs r), rhs = f (rhs r) }

map :: (f -> f') -> (v -> v') -> Rule f v -> Rule f' v'
map f v = mapSides (T.map f v)

