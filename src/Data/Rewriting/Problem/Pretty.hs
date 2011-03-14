module Data.Rewriting.Problem.Pretty (
    prettyProblem,
) where                        

import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import Data.Rewriting.Problem.Type
import Data.Rewriting.Rule (prettyRule)
import Text.PrettyPrint.ANSI.Leijen

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
  
  printWhen False _ = empty
  printWhen True  p = p
  
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