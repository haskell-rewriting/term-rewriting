{-# LANGUAGE TypeSynonymInstances #-}
module Data.Rewriting.Substitution.Type (
    Subst,
    GSubst,
    fromMap,
    toMap,
) where

import Data.Rewriting.Term.Type
import qualified Data.Map as M
import Text.PrettyPrint.ANSI.Leijen ((<>), encloseSep, lbrace, rbrace, comma, text, Pretty(..), Doc)

-- | A substitution, mapping variables to terms. Substitutions are
-- equal to the identity almost everywhere.
type Subst f v = GSubst v f v

-- | A generalised? substitution: a finite, partial map from variables
-- to terms with a different variable type.
newtype GSubst v f v' = GS { unGS :: M.Map v (Term f v') }
    deriving Show

-- Do not derive Eq: Depending on the interpretation,  v / Var v
-- will have to be ignored or not.

fromMap :: M.Map v (Term f v') -> GSubst v f v'
fromMap = GS

toMap :: GSubst v f v' -> M.Map v (Term f v')
toMap = unGS

prettyGSubst :: (v -> Doc) -> (f -> Doc) -> (v' -> Doc) -> GSubst v f v' -> Doc
prettyGSubst prettyVarDom  prettyFuns prettyVarImg subst = 
    encloseSep lbrace rbrace comma [ppBinding v t | (v,t) <- M.toList $ unGS subst]
    where ppBinding v t = prettyVarDom v <> text "/" <> prettyTerm prettyFuns prettyVarImg t

prettySubst :: (f -> Doc) -> (v -> Doc) -> Subst f v -> Doc
prettySubst prettyFuns prettyVars = prettyGSubst prettyVars prettyFuns prettyVars

instance (Pretty v, Pretty f, Pretty v') => Pretty (GSubst v f v') where
    pretty = prettyGSubst pretty pretty pretty

instance (Pretty f, Pretty v) => Pretty (Subst f v) where
    pretty = prettySubst pretty pretty
