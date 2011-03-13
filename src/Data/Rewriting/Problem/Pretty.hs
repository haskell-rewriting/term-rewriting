module Data.Rewriting.Problem.Pretty (
    prettyProblem,
) where                        

import Data.Maybe (isJust)
import Data.List (nub)
import Data.Rewriting.Problem.Type
import Data.Rewriting.Rule (prettyRule)
import Text.PrettyPrint.ANSI.Leijen

prettyProblem :: (Eq f, Eq v) => (f -> Doc) -> (v -> Doc) -> Problem f v -> Doc
prettyProblem fun var prob =  block "Start-Terms" (ppST `on` startTerms) 
                              <$$> block "Strategy" (ppStrat `on` strategy) 
                              <$$> block "Variables" (ppVars `on` variables) 
                              <$$> block "Function Symbols" (ppSyms `on` symbols) 
--                              <$$> printWhen (isJust `on` comment) (block "Comment" (ppComment `on` comment)) 
                              <$$> block "Rules" (ppRules `on` rules) where
  pp `on` fld = pp $ fld prob
  block n pp = hang 3 $ (underline $ text $ n ++ ":") <+> pp
  commalist  = fillSep . punctuate (text ",")
  
  printWhen False _ = empty
  printWhen True  p = p
  
  ppST TermAlgebra   = text "all"
  ppST BasicTerms    = text "basic terms"
  ppStrat Innermost  = text "innermost"
  ppStrat Outermost  = text "outermost"
  ppStrat Full       = text "full rewriting"
  ppVars vars        = commalist $ [var v | v <- nub vars]
  ppSyms syms        = commalist $ [fun v | v <- nub syms]
  ppComment (Just c) = text c
  ppComment Nothing  = empty
  ppRules rp         = braces $ align $ vcat $
                       [ppRule "->" r | r <- strictRules rp] 
                       ++ [ppRule "->=" r | r <- weakRules rp]
  ppRule sep         = prettyRule (text sep) fun var
instance (Eq f, Eq v, Pretty f, Pretty v) => Pretty (Problem f v) where
  pretty = prettyProblem pretty pretty