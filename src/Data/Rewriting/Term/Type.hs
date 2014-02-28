-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer, Ilya Epifanov

module Data.Rewriting.Term.Type (
    Term (..),
    fold,
    map,
) where

import Prelude hiding (map)
import qualified Prelude (map)

data Term f v
    = Var v            -- ^ Variable
    | Fun f [Term f v] -- ^ Function application
    deriving (Show, Eq)

instance (Ord f, Ord v) => Ord (Term f v) where
  compare t1 t2 = case (size t1) `compare` (size t2) of
    EQ -> case (t1, t2) of
      (Var v1, Var v2) -> compare v1 v2
      (Fun f1 [], Fun f2 []) -> compare f1 f2
      (Fun _ [], _) -> LT
      (_, Fun _ []) -> GT
      (Var _, _) -> LT
      (_, Var _) -> GT
      (Fun f1 _, Fun f2 _) -> compare f1 f2
    c -> c

-- | Folding terms.
--
-- >>> fold (\v -> 1) (\f xs -> 1 + sum xs) (Fun 'f' [Var 1, Fun 'g' []])
-- 3 -- size of the given term
fold :: (v -> a) -> (f -> [a] -> a) -> Term f v -> a
fold var fun (Var v) = var v
fold var fun (Fun f ts) = fun f (fmap (fold var fun) ts)

-- | Mapping terms: Rename function symbols and variables.
--
-- >>> map succ pred (Fun 'f' [Var 2, Fun 'g' []])
-- Fun 'e' [Var 3,Fun 'f' []]
map :: (f -> f') -> (v -> v') -> Term f v -> Term f' v'
map fun var = fold (Var . var) (Fun . fun)

-- | We'll call the total count of node in a term 'size'
size :: Term f v -> Int
size Var{} = 1
size (Fun _ t) = foldl (+) 1 $ Prelude.map size t
