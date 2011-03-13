module Data.Rewriting.Problem.Pretty (
    prettyProblem,
) where                        

import Data.Maybe (isJust)
import Data.Rewriting.Problem.Type
import Data.Rewriting.Rule (prettyRule)
import Text.PrettyPrint.ANSI.Leijen

prettyProblem :: (f -> Doc) -> (v -> Doc) -> Problem f v -> Doc
prettyProblem fun var prob =  block "Start terms" (ppST `on` startTerms) 
                              <$> block "Strategy" (ppStrat `on` strategy) 
                              <$> block "Variables" (ppVars `on` variables) 
                              <$> block "Function Symbols" (ppSyms `on` symbols) 
                              <$> printWhen (isJust `on` comment) (block "Comment" (ppComment `on` comment)) 
                              <$> block "Rules" (ppRules `on` rules) where
  pp `on` fld = pp $ fld prob
  block n pp = fillBreak 15 (text $ n ++ ":") <+> pp
  commalist  = encloseSep empty empty comma
  
  printWhen False _ = empty
  printWhen True  p = p
  
  ppST TermAlgebra   = text "all"
  ppST BasicTerms    = text "basic terms"
  ppStrat Innermost  = text "innermost"
  ppStrat Outermost  = text "outermost"
  ppStrat Full       = text "full rewriting"
  ppVars vars        = commalist [var v | v <- vars]
  ppSyms syms        = commalist [fun v | v <- syms]
  ppComment (Just c) = text c
  ppComment Nothing  = empty
  ppRules rp         = semiBraces $ 
                       [prettyRule (text "->") fun var r | r <- strictRules rp] 
                       ++ [prettyRule (text "->=") fun var r | r <- weakRules rp]
                       
instance (Pretty f, Pretty v) => Pretty (Problem f v) where
  pretty = prettyProblem pretty pretty