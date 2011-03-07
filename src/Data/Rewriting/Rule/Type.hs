module Data.Rewriting.Rule.Type (
    module Data.Rewriting.Term.Type,
    Rule (..),
    prettyRule 
) where

import Data.Rewriting.Term.Type
import Text.PrettyPrint.ANSI.Leijen ((<+>), (</>), text, Doc, Pretty(..))

-- | Rewrite rule with left-hand side and right-hand side.
data Rule f v = Rule { lhs :: Term f v, rhs :: Term f v }

-- mapRule :: (Term f v -> Term f' v') -> Rule f v -> Rule f' v'
-- mapRule f r = Rule{ lhs = f (lhs r), rhs = f (rhs r) }

prettyRule :: (f -> Doc) -> (v -> Doc) -> Rule f v -> Doc
prettyRule prettyFuns prettyVars (Rule l r) = ppTerm l </> text "->" <+> ppTerm r
    where ppTerm = prettyTerm prettyFuns prettyVars

instance (Pretty f, Pretty v) => Pretty (Rule f v) where
    pretty = prettyRule pretty pretty

