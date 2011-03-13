module Data.Rewriting.Problem.Type (
  StartTerms (..), 
  Strategy (..), 
  RulesPair (..),
  Problem (..), 
  Theory (..),
  allRules
 ) where 

import Data.Rewriting.Rule (Rule (..))

data StartTerms = TermAlgebra 
                | BasicTerms deriving (Eq, Show)
                                      
data Strategy = Innermost
              | Full
              | Outermost deriving (Eq, Show)

data RulesPair f v = RulesPair { strictRules :: [Rule f v]
                               , weakRules   :: [Rule f v] } deriving (Eq, Show)


data Theory = SymbolProperty [String]
            | Equations [Rule String String] deriving (Eq, Show)

data Problem f v = Problem { startTerms :: StartTerms
                           , strategy   :: Strategy
                           , theory     :: [Theory]
                           , rules      :: RulesPair f v
                           , variables  :: [v]
                           , symbols    :: [f] 
                           , comment    :: Maybe String} deriving (Show)

allRules :: RulesPair f v -> [Rule f v]
allRules rp = strictRules rp ++ weakRules rp
