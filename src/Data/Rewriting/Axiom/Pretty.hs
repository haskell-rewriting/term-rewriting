-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Ilya Epifanov

module Data.Rewriting.Axiom.Pretty (
    prettyAxiom
)  where

import Data.Rewriting.Axiom.Type
import Data.Rewriting.Term (prettyTerm)

import Text.PrettyPrint.ANSI.Leijen

prettyAxiom :: Doc -> (f -> Doc) -> (v -> Doc) -> Axiom f v -> Doc
prettyAxiom eq fun var (Axiom t1 t2) = hang 2 $ term t1 <+> eq </> term t2 where
    term = prettyTerm fun var

instance (Pretty f, Pretty v) => Pretty (Axiom f v) where
    pretty = prettyAxiom (text "≡") pretty pretty
