-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini

module Data.Rewriting.Problem.Pretty (
    prettyProblem,
    prettyWST,
    prettyWST',
) where

import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import Data.Rewriting.Problem.Type
import Data.Rewriting.Rule (prettyRule)
import Text.PrettyPrint.ANSI.Leijen

printWhen :: Bool -> Doc -> Doc
printWhen False _ = empty
printWhen True  p = p


prettyWST' :: (Pretty f, Pretty v) => Problem f v -> Doc
prettyWST' = prettyWST pretty pretty

prettyWST :: (f -> Doc) -> (v -> Doc) -> Problem f v -> Doc
prettyWST fun var prob =
    printWhen (sterms /= AllTerms) (block "STARTTERM" $ text "CONSTRUCTOR-BASED")
    <> printWhen (strat /= Full) (block "STRATEGY" $ ppStrat strat)
    <> maybeblock "THEORY" theory ppTheories
    <> block "VAR" (ppVars $ variables prob)
    <> maybeblock "SIG" signature ppSignature
    <> block "RULES" (ppRules $ rules prob)
    <> maybeblock "COMMENT" comment text

  where block n pp = (parens $ (hang 3 $ text n <$$> pp) <> linebreak) <> linebreak
        maybeblock n f fpp = case f prob of
                               Just e -> block n (fpp e)
                               Nothing -> empty

        ppStrat Innermost = text "INNERMOST"
        ppStrat Outermost = text "OUTERMOST"

        ppVars vs = align $ fillSep [ var v | v <- vs]

        ppTheories thys = align $ vcat [ppThy thy | thy <- thys]
            where ppThy (SymbolProperty p fs) = block p (align $ fillSep [ fun f | f <- fs ])
                  ppThy (Equations rs)        = block "EQUATIONS" $ vcat [ppRule "==" r | r <- rs]

        ppSignature sigs = align $ fillSep [ppSig sig | sig <- sigs]
            where ppSig (f,i) = parens $ fun f <+> int i

        ppRules rp = align $ vcat ([ppRule "->" r | r <- strictRules rp]
                                   ++ [ppRule "->=" r | r <- weakRules rp])

        ppRule sep = prettyRule (text sep) fun var

        sterms = startTerms prob
        strat  = strategy prob
        thry   = theory prob


prettyProblem :: (Eq f, Eq v) => (f -> Doc) -> (v -> Doc) -> Problem f v -> Doc
prettyProblem fun var prob =  block "Start-Terms" (ppST `on` startTerms)
                              <$$> block "Strategy" (ppStrat `on` strategy)
                              <$$> block "Variables" (ppVars `on` variables)
                              <$$> block "Function Symbols" (ppSyms `on` symbols)
                              <$$> maybeblock "Theory" ppTheories theory
                              <$$> block "Rules" (ppRules `on` rules)
                              <$$> maybeblock "Comment" ppComment comment where
  pp `on` fld = pp $ fld prob
  block n pp = hang 3 $ (underline $ text $ n ++ ":") <+> pp
  maybeblock n pp f = printWhen (isJust `on` f) (block n (pp `on` (fromJust . f)))
  commalist  = fillSep . punctuate (text ",")

  ppST AllTerms      = text "all"
  ppST BasicTerms    = text "basic terms"
  ppStrat Innermost  = text "innermost"
  ppStrat Outermost  = text "outermost"
  ppStrat Full       = text "full rewriting"
  ppVars vars        = commalist $ [var v | v <- nub vars]
  ppSyms syms        = commalist $ [fun v | v <- nub syms]
  ppComment c        = text c
  ppTheories ths     =  align $ vcat [ ppTheory th | th <- ths ] where
      ppTheory (SymbolProperty p fs) = text (p++":") <+> align (commalist [ fun f | f <- fs])
      ppTheory (Equations rs)        = align $ vcat [ppRule "==" r | r <- rs]
  ppRules rp         = align $ vcat $
                       [ppRule "->" r | r <- strictRules rp]
                       ++ [ppRule "->=" r | r <- weakRules rp]
  ppRule sep         = prettyRule (text sep) fun var

instance (Eq f, Eq v, Pretty f, Pretty v) => Pretty (Problem f v) where
  pretty = prettyProblem pretty pretty
