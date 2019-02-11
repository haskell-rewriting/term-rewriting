-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini, Christial Sternagel

module Data.Rewriting.Problem.Type (
  StartTerms (..),
  Strategy (..),
  RulesPair (..),
  Problem (..),
  Theory (..),
  allRules,
  map
 ) where

import Prelude hiding (map)
import qualified Prelude as P

import Data.Rewriting.Rule (Rule (..))
import qualified Data.Rewriting.Rule as Rule

data StartTerms = AllTerms
                | BasicTerms deriving (Eq, Show)

data Strategy = Innermost
              | Full
              | Outermost deriving (Eq, Show)

data RulesPair f v = RulesPair { strictRules :: [Rule f v]
                               , weakRules   :: [Rule f v] } deriving (Eq, Show)


data Theory f v = SymbolProperty String [f]
                | Equations [Rule f v] deriving (Eq, Show)

data Problem f v = Problem { startTerms :: StartTerms
                           , strategy   :: Strategy
                           , theory     :: Maybe [Theory f v]
                           , rules      :: RulesPair f v
                           , variables  :: [v]
                           , symbols    :: [f]
                           , signature  :: Maybe [(f, Int)]
                           , comment    :: Maybe String} deriving (Show)

allRules :: RulesPair f v -> [Rule f v]
allRules rp = strictRules rp ++ weakRules rp

map :: (f -> f') -> (v -> v') -> Problem f v -> Problem f' v'
map ffun fvar prob = 
   Problem { startTerms = startTerms prob 
           , strategy = strategy prob
           , theory = P.map (mapTheory ffun fvar) <$> theory prob 
           , rules = mapRulesPair ffun fvar (rules prob)
           , variables = P.map fvar (variables prob)
           , symbols = P.map ffun (symbols prob)
           , signature = fmap (P.map (\(f, a) -> (ffun f, a))) (signature prob)
           , comment = comment prob}
              
mapTheory :: (f -> f') -> (v -> v') -> Theory f v -> Theory f' v'
mapTheory ffun _ (SymbolProperty p fs) = SymbolProperty p (P.map ffun fs)
mapTheory ffun fvar (Equations rs) = Equations (P.map (Rule.map ffun fvar) rs)

mapRulesPair :: (f -> f') -> (v -> v') -> RulesPair f v -> RulesPair f' v'
mapRulesPair ffun fvar rp = 
    RulesPair { strictRules = modify (strictRules rp)
              , weakRules = modify (weakRules rp)}
        where modify = P.map (Rule.map ffun fvar)
