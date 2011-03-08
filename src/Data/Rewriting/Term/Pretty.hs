module Data.Rewriting.Term.Pretty (
    prettyTerm,
) where                        

import Data.Rewriting.Term.Type
import Text.PrettyPrint.ANSI.Leijen


prettyTerm :: (f -> Doc) -> (v -> Doc) -> Term f v -> Doc
prettyTerm _         var (Var x) = var x
prettyTerm fun var (Fun f ts)    = fun f <> args where
    args = encloseSep lparen rparen comma [prettyTerm fun var ti | ti <- ts]

instance (Pretty f, Pretty v) => Pretty (Term f v) where
    pretty = prettyTerm pretty pretty
