-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini

module Data.Rewriting.Rule.Pretty (
    prettyRule
)  where

import Data.Rewriting.Rule.Type
import Data.Rewriting.Term (prettyTerm)

import Text.PrettyPrint.ANSI.Leijen

prettyRule :: Doc -> (f -> Doc) -> (v -> Doc) -> Rule f v -> Doc
prettyRule arr fun var (Rule l r) = hang 2 $ term l <+> arr </> term r where
    term = prettyTerm fun var

instance (Pretty f, Pretty v) => Pretty (Rule f v) where
    pretty = prettyRule (text "->") pretty pretty

