-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

module Data.Rewriting.Substitution.Type (
    Subst,
    GSubst,
    -- * utilities not reexported from 'Data.Rewriting.Substitution'
    fromMap,
    toMap,
) where

import Data.Rewriting.Term.Type
import qualified Data.Map as M


-- | A substitution, mapping variables to terms. Substitutions are
-- equal to the identity almost everywhere.
type Subst f v = GSubst v f v

-- | A generalised? substitution: a finite, partial map from variables
-- to terms with a different variable type.
newtype GSubst v f v' = GS { unGS :: M.Map v (Term f v') }
    deriving Show

-- Do not derive Eq: Depending on the interpretation,  v / Var v
-- will have to be ignored or not.

fromMap :: M.Map v (Term f v') -> GSubst v f v'
fromMap = GS

toMap :: GSubst v f v' -> M.Map v (Term f v')
toMap = unGS
