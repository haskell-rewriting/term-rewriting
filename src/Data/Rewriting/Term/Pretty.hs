module Data.Rewriting.Term.Pretty (
   prettyTerm,
) where                        

import Data.Rewriting.Term.Type
import Text.PrettyPrint.ANSI.Leijen


prettyTerm :: (f -> Doc) -> (v -> Doc) -> Term f v -> Doc
prettyTerm _          prettyVars (Var x)    = prettyVars x
prettyTerm prettyFuns prettyVars (Fun f ts) = prettyFuns f <> prettyArgs
    where prettyArgs = encloseSep lparen rparen comma [prettyTerm prettyFuns prettyVars ti | ti <- ts]

instance (Pretty f, Pretty v) => Pretty (Term f v) where
    pretty = prettyTerm pretty pretty


