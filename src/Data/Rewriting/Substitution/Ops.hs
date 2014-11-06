-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Christian Sternagel

module Data.Rewriting.Substitution.Ops (
    apply,
    applyRule,
    applyCtxt,
    gApply,
    compose,
    merge,
) where

import Data.Rewriting.Substitution.Type
import qualified Data.Rewriting.Term.Type as Term
import Data.Rewriting.Term.Type (Term (..))
import Data.Rewriting.Rule.Type (Rule (..))
import Data.Rewriting.Context.Type (Ctxt (..))
import qualified Data.Map as M
import Control.Monad
import Control.Applicative

-- | Apply a substitution, assuming that it's the identity on variables not
-- mentionend in the substitution.
apply :: (Ord v) => Subst f v -> Term f v -> Term f v
apply subst = Term.fold var fun where
    var v = M.findWithDefault (Var v) v (toMap subst)
    fun = Fun

-- | Liftting of 'apply' to rules: applies the given substitution to left- and right-hand side.
applyRule :: (Ord v) => Subst f v -> Rule f v -> Rule f v
applyRule subst rl = Rule (apply subst (lhs rl)) (apply subst (rhs rl))

-- | Liftting of 'apply' to contexts.
applyCtxt :: Ord v => Subst f v -> Ctxt f v -> Ctxt f v
applyCtxt _ Hole = Hole
applyCtxt subst (Ctxt f ts1 ctxt ts2) =
  Ctxt f (map (apply subst) ts1) (applyCtxt subst ctxt) (map (apply subst) ts2)


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
