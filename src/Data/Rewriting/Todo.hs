module Data.Rewriting.Todo where

import Data.Rewriting.Substitution
import Data.Rewriting.Term.Type as Term
import qualified Data.Map as M
import Control.Monad

-- | Apply a substitution, assuming that it's the identity on variables not
-- mentionend in the substitution.
apply :: (Ord v) => Substitution f v -> Term f v -> Term f v
apply subst = Term.fold var fun where
   var v = M.findWithDefault (Var v) v subst
   fun = Fun

-- | Apply a substitution, assuming that it's total; variables not occurring
-- in the substitution are replaced by bottom.
apply' :: (Ord v) => Substitution' v f v' -> Term f v -> Term f v'
apply' subst = Term.fold var fun where
   var v = M.findWithDefault (error "apply': unknown variable") v subst
   fun = Fun

match :: (Eq f, Ord v, Eq v') => Term f v -> Term f v' -> Maybe (Substitution' v f v')
match t u = go t u (M.empty) where
   go (Var v) t subst = case M.lookup v subst of
       Nothing -> Just (M.insert v t subst)
       Just t' | t == t' -> Just subst
       _ -> Nothing
   go (Fun f ts) (Fun f' ts') subst
       | f /= f' || length ts /= length ts' = Nothing
       | otherwise = composeM (zipWith go ts ts') subst
   go _ _ _ = Nothing

composeM :: Monad m => [a -> m a] -> a -> m a
composeM = foldr (>=>) return

unify :: (Ord f, Ord v) => Term f v -> Term f v -> Maybe (Substitution f v)
unify = undefined
