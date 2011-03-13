module Data.Rewriting.Rules (
    module Data.Rewriting.Rules.Rewrite, 
    funsDL
) where

import Data.Rewriting.Rules.Rewrite hiding (nested, listContexts)
import Data.Rewriting.Rule (Rule)
import qualified Data.Rewriting.Rule as Rule

funsDL :: [Rule f v] -> [f] -> [f]
funsDL rs fs = foldr Rule.funsDL fs rs