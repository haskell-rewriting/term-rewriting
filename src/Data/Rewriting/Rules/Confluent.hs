-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Ilya Epifanov

{-# LANGUAGE FlexibleContexts #-}

module Data.Rewriting.Rules.Confluent (
  knuthBendix,
  makeConfluent
) where

import Data.Rewriting.Problem as Problem
import Data.Rewriting.CriticalPair as CP (cps', toAxiom)
import Data.Rewriting.Rule as Rule (Rule(..), canonify, isTrivial)
import Data.Rewriting.Rules.Rewrite (reduceRule, reduceAxiom)
import Data.Rewriting.Term as Term (Term)
import Data.Rewriting.Axiom as Axiom
import qualified Data.Rewriting.Term as Term
import qualified Data.Rewriting.Rule as Rule
import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace
import Control.Applicative
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Control.Applicative

reduceAndOrient :: (Ord (Term f v), Ord f, Eq f, Ord v) => [Rule f v] -> Axiom f v -> Rule f v
reduceAndOrient rs a = Axiom.orient $ reduceAxiom rs a

addCompletions :: (Pretty f, Pretty v, Show f, Show v, Eq f, Ord f, Ord v)
                  => [v] -> [Rule f v] -> [Rule f v]
addCompletions vs rs = trace "-- adding completions --" $! foldl addCompletion [] $ rs ++ (orient . CP.toAxiom <$> cps' rs)
  where addCompletion rs' r'
          | Rule.isTrivial r'' = trace ("trivial: " ++ pp_ r'') rs'
          | r'' `elem` rs' = trace ("elem: " ++ pp_ r'') rs'
          | otherwise = trace ("new: " ++ pp_ r'') $ r'':rs'
          where r'' = Rule.canonify vs $ reduceRule rs' r'

knuthBendix :: (Pretty f, Pretty v, Show f, Show v, Ord (Term f v), Ord f, Eq f, Ord v) => [v] -> [Axiom f v] -> [Rule f v]
knuthBendix vs as = inner as []
  where
--    inner :: [Axiom f v] -> [Rule f v] -> [Rule f v]
    inner (a:as') rs = inner as' $ addCompletions vs $ reduceAndOrient rs a:rs
    inner [] rs = rs

makeConfluent :: (Pretty f, Pretty v, Show f, Show v, Ord f, Ord v) => Problem f v -> Problem f v
makeConfluent p =
  p {rules = RulesPair (knuthBendix (variables p) (Axiom.fromRule <$> (allRules $ rules p))) [] }

pp' :: Doc -> String
pp' p = displayS (renderPretty 0.8 120 p) ""

pp_ :: (Pretty p) => p -> String
pp_ p = pp' $ pretty p
