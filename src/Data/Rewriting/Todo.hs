-- | Apply a substitution, assuming that it's the identity on variables not
-- mentionend in the substitution.
apply :: Substitution f v -> Term f v -> Term f v
apply = undefined

-- | Apply a substitution, assuming that it's total; variables not occurring
-- in the substitution are replaced by bottom.
apply' :: Substitution' v' f v -> Term f v' -> Term f v
apply' = undefined

match :: (Eq f, Ord v') => Term f v' -> Term f v -> Maybe (Substitution' v' f v)
match = undefined

unify :: (Ord f, Ord v) => Term f v -> Term f v -> Maybe (Substitution f v)
unify = undefined
