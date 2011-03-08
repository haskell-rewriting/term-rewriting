module Data.Rewriting.Rule.Pretty (
    prettyRule
)  where

import Data.Rewriting.Rule.Type
import Data.Rewriting.Term (prettyTerm)

import Text.PrettyPrint.ANSI.Leijen

prettyRule :: (f -> Doc) -> (v -> Doc) -> Rule f v -> Doc
prettyRule fun var (Rule l r) = term l </> text "->" <+> term r where
    term = prettyTerm fun var

instance (Pretty f, Pretty v) => Pretty (Rule f v) where
    pretty = prettyRule pretty pretty

