-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Ilya Epifanov

module Data.Rewriting.Axiom.Type (
    Axiom (..),
) where

import Data.Rewriting.Term.Type

-- | An axiom is just a declaration of identity
data Axiom f v = Axiom {
    t1 :: Term f v, -- ^ one side of an equivalence
    t2 :: Term f v  -- ^ another side of an equivalence
}
