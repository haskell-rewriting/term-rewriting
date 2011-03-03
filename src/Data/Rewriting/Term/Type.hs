module Data.Rewriting.Term.Type (
    Term (..),
    foldTerm,
    mapTerm,
) where

data Term f v = Var v | Fun f [Term f v]
    deriving (Show, Eq, Ord)

foldTerm :: (v -> a) -> (f -> [a] -> a) -> Term f v -> a
foldTerm var fun (Var v)    = var v
foldTerm var fun (Fun f ts) = fun f (map (foldTerm var fun) ts)

mapTerm :: (v -> v') -> (f -> f') -> Term f v -> Term f' v'
mapTerm var fun = foldTerm (Var . var) (Fun . fun)
