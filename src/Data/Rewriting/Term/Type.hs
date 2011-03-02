module Data.Rewriting.Term.Type (
    Term (..),
    foldTerm,
    mapTerm,
) where

data Term f v
    = Var v            -- ^ Variable
    | Fun f [Term f v] -- ^ Function application
    deriving (Show, Eq, Ord)

-- | Folding terms.
--
-- >>> foldTerm (\v -> 1) (\f xs -> 1 + sum xs) (Fun 'f' [Var 1, Fun 'g' []])
-- 3 -- size of the given term
foldTerm :: (v -> a) -> (f -> [a] -> a) -> Term f v -> a
foldTerm var fun (Var v) = var v
foldTerm var fun (Fun f ts) = fun f (map (foldTerm var fun) ts)

-- | Mapping terms: Rename function symbols and variables.
--
-- >>> mapTerm succ pred (Fun 'f' [Var 2, Fun 'g' []])
-- Fun 'e' [Var 3,Fun 'f' []]
mapTerm :: (v -> v') -> (f -> f') -> Term f v -> Term f' v'
mapTerm var fun = foldTerm (Var . var) (Fun . fun)
