apply :: Substitution f v -> Term f v -> Term f v
apply = undefined

-- or -> Maybe (Term f v)?
apply' :: Substitution' v' f v -> Term f v' -> Term f v
apply' = undefined

match :: (Eq f, Ord v') => Term f v' -> Term f v -> Substitution' v' f v
match = undefined

unify :: (Ord f, Ord v) => Term f v -> Term f v -> Substitution f v
unify = undefined
