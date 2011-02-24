-- names following Data.Map and Data.Set
module Data.Multiset (
    Multiset,
    fromElems,
    fromList,
    toList,
    member,
    lookup,
    union,
    difference,
    intersection,
    isSubsetOf,
) where

import qualified Data.Map as M
import Data.Maybe
import Prelude hiding (lookup)

newtype Multiset a = MS { unMS :: M.Map a Int }
    deriving (Eq, Ord)

fromElems :: Ord a => [a] -> Multiset a
fromElems = fromList . map (\x -> (x, 1))

fromList :: Ord a => [(a, Int)] -> Multiset a
fromList = MS . M.fromListWith (+)

toList :: Ord a => Multiset a -> [(a, Int)]
toList = M.toList . unMS

member :: Ord a => a -> Multiset a -> Bool
member a = isJust . M.lookup a . unMS

lookup :: Ord a => a -> Multiset a -> Int
lookup a = fromMaybe 0 . M.lookup a . unMS

union :: Ord a => Multiset a -> Multiset a -> Multiset a
union ma mb = MS $ M.unionWith (+) (unMS ma) (unMS mb)

difference :: Ord a => Multiset a -> Multiset a -> Multiset a
difference ma mb = MS $ M.differenceWith
    (\a b -> if a > b then Just (a - b) else Nothing)
    (unMS ma) (unMS mb)

intersection :: Ord a => Multiset a -> Multiset a -> Multiset a
intersection ma mb = MS $ M.differenceWith
    (\a b -> let c = max a b in if c > 0 then Just c else Nothing)
    (unMS ma) (unMS mb)

isSubsetOf :: Ord a => Multiset a -> Multiset a -> Bool
isSubsetOf ma mb = M.isSubmapOfBy (<=) (unMS ma) (unMS mb)
