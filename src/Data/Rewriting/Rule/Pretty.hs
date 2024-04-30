-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini

module Data.Rewriting.Rule.Pretty (
    prettyRule
)  where

import Data.Rewriting.Rule.Type
import Data.Rewriting.Term (prettyTerm)

--import Text.PrettyPrint.ANSI.Leijen
import Prettyprinter

prettyRule :: Doc ann -> (f -> Doc ann) -> (v -> Doc ann) -> Rule f v -> Doc ann
prettyRule arr fun var (Rule l r) = hang 2 $ term l <+> arr <> softline <> term r where
    term = prettyTerm fun var

instance (Pretty f, Pretty v) => Pretty (Rule f v) where
    pretty = prettyRule (pretty "->") pretty pretty

