{-# OPTIONS_GHC -XFlexibleContexts#-}
module Data.Rewriting.Term.Parse (
  fromString,
  parse,
  parseIO,
  parseWST
) where

import Prelude hiding (lex)
import Control.Monad
import Data.Rewriting.Term
import Data.Char
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

{-| Like @fromString@, but the result is wrapped in the IO monad, making this
function useful for interactive testing.

>>> parseIO ["x","y"] "f(x,c)"
Fun "f" [Var "x",Fun "c" []]
-}
parseIO :: [String] -> String -> IO (Term String String)
parseIO xs input = case fromString xs input of
  Left err -> do { putStr "parse error at "; print err; mzero }
  Right t  -> return t

{-| @fromString xs s@ parsers a term from the string @s@, where elements of @xs@
are considered as variables. -}
fromString :: [String] -> String -> Either ParseError (Term String String)
fromString xs input = Parsec.parse (all $ parseWST xs) "" input
  where all = between spaces (spaces >> eof)


{-| A parser for terms, where @funP@ and @varP@ are parsers for function symbols
and variables, respectively. The @varP@ parser has a higher priority than the
@funP@ parser. Hence, whenever @varP@ succeeds, the token is treated as a
variable.

Note that the user has to take care of handling trailing white space in
@funP@ and @varP@.
 -}
parse :: Stream s m Char => ParsecT s u m f -> ParsecT s u m v
  -> ParsecT s u m (Term f v)
parse funP varP = term <?> "term"
  where
    term = try (liftM Var varP) <|> liftM2 Fun funP args
    args = between (lex $ char '(') (lex $ char ')') (sepBy term (lex $ char ','))
             <|> return []


{-| A parser for terms following the conventions of the ancient ASCII input
format for the termination competition: every @Char@ that is neither a white
space (according to @Data.Char.isSpace@) nor one of @'('@, @')'@, or @','@,
is considered a letter. An identifier is a non-empty sequence of letters and
it is treated as variable iff it is contained in @xs@.
 -}
-- change name?
parseWST :: Stream s m Char => [String] -> ParsecT s u m (Term String String)
parseWST xs = parse funP varP
  where
    funP  = lex ident
    varP  = do { x <- lex ident; guard (x `elem` xs); return x }
    ident = many1 (satisfy (\c -> not (isSpace c) && not (c `elem` "(),")))


{- Same as @p@ but also consume trailing white space. -}
lex p = do { x <- p; spaces; return x }
