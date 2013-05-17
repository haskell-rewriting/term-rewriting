module Data.Rewriting.Substitution.Ops (
    apply,
    gApply,
    compose,
    merge,
) where

import Data.Rewriting.Substitution.Type
import qualified Data.Rewriting.Term.Type as Term
import Data.Rewriting.Term.Type (Term (..))
import qualified Data.Map as M
import Control.Monad
import Control.Applicative

-- | Apply a substitution, assuming that it's the identity on variables not
-- mentionend in the substitution.
apply :: (Ord v) => Subst f v -> Term f v -> Term f v
apply subst = Term.fold var fun where
    var v = M.findWithDefault (Var v) v (toMap subst)
    fun = Fun

-- | Apply a substitution, assuming that it's total. If the term contains
-- a variable not defined by the substitution, return 'Nothing'.
gApply :: (Ord v) => GSubst v f v' -> Term f v -> Maybe (Term f v')
gApply subst = Term.fold var fun where
    var v = M.lookup v (toMap subst)
    fun f ts = Fun f <$> sequence ts

-- | Compose substitutions. We have
--
-- > (s1 `compose` s2) `apply` t = s1 `apply` (s2 `apply` t).
compose :: (Ord v) => Subst f v -> Subst f v -> Subst f v
compose subst subst' =
    fromMap (M.unionWith const (apply subst <$> toMap subst') (toMap subst))

-- | Merge two substitutions. The operation fails if some variable is
-- different terms by the substitutions.
merge :: (Ord v, Eq f, Eq v')
    => GSubst v f v' -> GSubst v f v' -> Maybe (GSubst v f v')
merge subst subst' = do
    guard $ and (M.elems (M.intersectionWith (==) (toMap subst) (toMap subst')))
    return $ fromMap $ M.union (toMap subst) (toMap subst')