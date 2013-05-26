-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Bertram Felgenhauer

-- Main test driver.

module Main (main) where

import qualified Pos
import qualified Term
import qualified Rule
import qualified CriticalPair
import qualified Substitution

import Test.QuickCheck
import Test.HUnit
import Control.Monad
import System.IO

properties :: [(String, Property)]
properties = [
    ("Pos.propParallelTo", property Pos.propParallelTo),
    ("Term.propReplaceAt1", property Term.propReplaceAt1),
    ("Term.propReplaceAt2", property Term.propReplaceAt2),
    ("Rule.propLeftRightLinearDual", property Rule.propLeftRightLinearDual),
    ("Rule.propCollapsingExpandingDual", property Rule.propCollapsingExpandingDual),
    ("Rule.propErasingCreatingDual", property Rule.propErasingCreatingDual),
    ("Rule.propLinear", property Rule.propLinear),
    ("Rule.propValid", property Rule.propValid),
    ("Substitution.propCompose", property Substitution.propCompose),
    ("Substitution.propUnify1", property Substitution.propUnify1),
    ("Substitution.propUnify2", property Substitution.propUnify2),
    ("CriticalPair.propValidCPs'", property CriticalPair.propValidCPs'),
    ("CriticalPair.propOuterCPs'", property CriticalPair.propOuterCPs'),
    ("CriticalPair.propInnerCPs'", property CriticalPair.propInnerCPs')
   ]

tests :: Test
tests = TestList [
    CriticalPair.tests,
    Rule.tests,
    TestList []]

main :: IO ()
main = do
    forM_ properties $ \(name, prop) -> do
        putStrLn $ "- " ++ name
        quickCheck prop
    putStrLn $ "- HUnit tests"
    runTestTT tests
    return ()
