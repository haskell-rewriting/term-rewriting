-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Auhtors: Martin Avanzini, Christian Sternagel

module Data.Rewriting.Term.Pretty (
    prettyTerm,
) where

import Data.Rewriting.Term.Type
import Text.PrettyPrint.ANSI.Leijen

-- | Given a pretty printer @f@ for function symbols and pretty printer @v@ for variables
-- @prettyTerm f v@ produces a pretty printer for terms

prettyTerm :: (f -> Doc) -> (v -> Doc) -> Term f v -> Doc
prettyTerm _         var (Var x) = var x
prettyTerm fun var (Fun f ts)    = fun f <> args where
    args = encloseSep lparen rparen comma [prettyTerm fun var ti | ti <- ts]

instance (Pretty f, Pretty v) => Pretty (Term f v) where
    pretty = prettyTerm pretty pretty
