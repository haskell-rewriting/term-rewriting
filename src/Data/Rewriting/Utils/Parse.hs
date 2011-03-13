{-# LANGUAGE FlexibleContexts#-}
module Data.Rewriting.Utils.Parse (
    lex,
    par,
    ident
) where

import Prelude hiding (lex)
import Text.Parsec
import Data.Char (isSpace)

-- | @lex p@ is similar to @p@ but also consumes trailing white space.
lex :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lex p = do { x <- p; spaces; return x }

-- | @par p@ accpets @p@ enclosed in parentheses ('@(@' and '@)@').
par :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
par = between (lex$char '(') (lex$char ')')

-- | @ident tabu@ parses a non-empty sequence of non-space characters not
-- containing elements of @tabu@.
ident :: Stream s m Char => String -> ParsecT s u m String
ident tabu = many1 (satisfy (\c -> not (isSpace c) && c `notElem` ("()," ++ tabu)))

