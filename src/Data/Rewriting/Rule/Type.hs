module Data.Rewriting.Rule.Type (
    module Data.Rewriting.Term.Type,
    Rule (..),
) where

import Data.Rewriting.Term.Type

-- | Rewrite rule with left-hand side and right-hand side.
data Rule f v = Rule { lhs :: Term f v, rhs :: Term f v }

-- mapRule :: (Term f v -> Term f' v') -> Rule f v -> Rule f' v'
-- mapRule f r = Rule{ lhs = f (lhs r), rhs = f (rhs r) }
