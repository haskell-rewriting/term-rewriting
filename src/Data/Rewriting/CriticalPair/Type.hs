-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

module Data.Rewriting.CriticalPair.Type (
    CP (..),
) where

import Data.Rewriting.Substitution
import Data.Rewriting.Rule.Type
import Data.Rewriting.Pos

-- | A critical pair. Critical pairs (should) have the following properties:
--
-- @
-- top   == Context.ofTerm top pos (Term.map Left id (Rule.lhs leftRule))
-- left  == Context.ofTerm top pos (Term.map Left id (Rule.rhs leftRule))
-- top   == Substitution.apply subst (Term.map Right id (Rule.lhs rightRule))
-- right == Substitution.apply subst (Term.map Right id (Rule.rhs rightRule))
-- @
--
-- Furthermore, @pos@ is a non-variable position of @(lhs rightRule)@ and
-- @subst@ is a most general substitution with these properties.
data CP f v v' = CP {
    left :: Term f (Either v v'),  -- ^ left reduct
    top :: Term f (Either v v'),   -- ^ source
    right :: Term f (Either v v'), -- ^ right reduct
    leftRule :: Rule f v,          -- ^ rule applied on left side
    leftPos :: Pos,                -- ^ position of left rule application
    rightRule :: Rule f v',        -- ^ rule applied on right side
    subst :: Subst f (Either v v') -- ^ common substitution of the rewrite steps
}
