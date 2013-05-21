-- Authors: Bertram Felgenhauer

module Data.Rewriting.Rule.Type (
    module Data.Rewriting.Term.Type,
    Rule (..),
) where

import Data.Rewriting.Term.Type hiding (map, fold)

-- | Rewrite rule with left-hand side and right-hand side.
data Rule f v = Rule { lhs :: Term f v, rhs :: Term f v }
    deriving (Ord, Eq, Show)

-- mapRule :: (Term f v -> Term f' v') -> Rule f v -> Rule f' v'
-- mapRule f r = Rule{ lhs = f (lhs r), rhs = f (rhs r) }

