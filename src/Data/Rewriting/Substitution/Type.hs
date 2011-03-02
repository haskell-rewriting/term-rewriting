module Data.Rewriting.Substitution.Type (
    Subst,
    GSubst,
    fromMap,
    toMap,
) where

import Data.Rewriting.Term.Type
import qualified Data.Map as M

type Subst f v = GSubst v f v

newtype GSubst v f v' = GS { unGS :: M.Map v (Term f v') }
    deriving Show

fromMap :: M.Map v (Term f v') -> GSubst v f v'
fromMap = GS

toMap :: GSubst v f v' -> M.Map v (Term f v')
toMap = unGS
