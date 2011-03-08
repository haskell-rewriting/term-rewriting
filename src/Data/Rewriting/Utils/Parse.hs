{-# LANGUAGE FlexibleContexts#-}
module Data.Rewriting.Utils.Parse (
    lex,
    par
) where

import Prelude hiding (lex)
import Text.Parsec

-- | @lex p@ is similar to @p@ but also consumes trailing white space.
lex :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lex p = do { x <- p; spaces; return x }

-- | @par p@ accpets @p@ enclosed in parentheses ('@(@' and '@)@').
par :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
par = between (lex$char '(') (lex$char ')')
