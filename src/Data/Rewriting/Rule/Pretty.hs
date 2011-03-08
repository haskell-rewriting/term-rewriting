module Data.Rewriting.Rule.Pretty (
    prettyRule
)  where

import Data.Rewriting.Rule.Type
import Data.Rewriting.Term (prettyTerm)

import Text.PrettyPrint.ANSI.Leijen

prettyRule :: (f -> Doc) -> (v -> Doc) -> Rule f v -> Doc
prettyRule prettyFuns prettyVars (Rule l r) = ppTerm l </> text "->" <+> ppTerm r
    where ppTerm = prettyTerm prettyFuns prettyVars

instance (Pretty f, Pretty v) => Pretty (Rule f v) where
    pretty = prettyRule pretty pretty

