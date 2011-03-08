{-# LANGUAGE FlexibleContexts#-}
module Data.Rewriting.Utils.Parse (
    lex,
    par
) where

import Prelude hiding (lex)
import Text.Parsec

-- Same as @p@ but also consume trailing white space.
lex :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lex p = do { x <- p; spaces; return x }

par :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
par = between (lex$char '(') (lex$char ')')
