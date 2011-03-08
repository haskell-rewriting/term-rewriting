{-# LANGUAGE TypeSynonymInstances #-}

module Data.Rewriting.Substitution.Pretty (
   prettySubst
) where 

import Data.Rewriting.Substitution.Type
import Data.Rewriting.Term (prettyTerm)
import qualified Data.Map as M

import Text.PrettyPrint.ANSI.Leijen

prettyGSubst :: (v -> Doc) -> (f -> Doc) -> (v' -> Doc) -> GSubst v f v' -> Doc
prettyGSubst prettyVarDom  prettyFuns prettyVarImg subst = 
    encloseSep lbrace rbrace comma [ppBinding v t | (v,t) <- M.toList $ toMap subst]
    where ppBinding v t = prettyVarDom v <> text "/" <> prettyTerm prettyFuns prettyVarImg t

prettySubst :: (f -> Doc) -> (v -> Doc) -> Subst f v -> Doc
prettySubst prettyFuns prettyVars = prettyGSubst prettyVars prettyFuns prettyVars

instance (Pretty v, Pretty f, Pretty v') => Pretty (GSubst v f v') where
    pretty = prettyGSubst pretty pretty pretty

instance (Pretty f, Pretty v) => Pretty (Subst f v) where
    pretty = prettySubst pretty pretty
