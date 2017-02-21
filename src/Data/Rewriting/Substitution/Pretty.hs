-- This file is part of the 'term-rewriting' library. It is licensed
-- under an MIT license. See the accompanying 'LICENSE' file for details.
--
-- Authors: Martin Avanzini

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Data.Rewriting.Substitution.Pretty (
    prettySubst
) where

import Data.Rewriting.Substitution.Type
import Data.Rewriting.Term (prettyTerm)
import qualified Data.Map as M

import Text.PrettyPrint.ANSI.Leijen

prettyGSubst :: (v -> Doc) -> (f -> Doc) -> (v' -> Doc) -> GSubst v f v' -> Doc
prettyGSubst var fun var' subst =
    encloseSep lbrace rbrace comma [ppBinding v t | (v,t) <- M.toList $ toMap subst]
    where ppBinding v t = var v <> text "/" <> prettyTerm fun var' t

prettySubst :: (f -> Doc) -> (v -> Doc) -> Subst f v -> Doc
prettySubst fun var = prettyGSubst var fun var

instance (Pretty v, Pretty f, Pretty v') => Pretty (GSubst v f v') where
    pretty = prettyGSubst pretty pretty pretty
