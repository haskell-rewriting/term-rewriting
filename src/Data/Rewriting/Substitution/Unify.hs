module Data.Rewriting.Substitution.Unify (
    unify,
) where

import Data.Rewriting.Substitution.Type
import Data.Rewriting.Substitution.Ops (apply)
import qualified Data.Rewriting.Term.Type as Term
import qualified Data.Rewriting.Term.Props as Term
import Data.Rewriting.Term.Type (Term (..))

import qualified Data.Map as M
import Control.Arrow
import Control.Applicative

unify :: (Eq f, Ord v) => Term f v -> Term f v -> Maybe (Subst f v)
unify t u = fromMap <$> go [(t, u)] M.empty where
   go [] subst = Just subst
   go ((t, u) : xs) subst = case (t, u) of
      (Var v, t) -> add v t xs subst
      (t, Var v) -> add v t xs subst
      (Fun f ts, Fun f' ts')
          | f /= f' || length ts /= length ts' -> Nothing
          | otherwise -> go (zip ts ts' ++ xs) subst
   add v t xs subst
       | Var v == t = go xs subst
       | occurs v t = Nothing
       | otherwise =
           let app = apply (fromMap (M.singleton v t))
           in  go (fmap (app *** app) xs) (M.insert v t (fmap app subst))
   occurs v t = v `elem` Term.vars t
