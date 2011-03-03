{-# OPTIONS_GHC -XFlexibleContexts#-}
module Data.Rewriting.Term.Parse (
  fromString,
  ident,
  lex,
  parse,
  parseIO,
  parseFun,
  parseVar,
  parseWST
) where

import Data.Rewriting.Utils
import Prelude hiding (lex)
import Control.Monad
import Data.Rewriting.Term
import Data.Char
import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec

-- | Like @fromString@, but the result is wrapped in the IO monad, making this
-- function useful for interactive testing.
--
-- >>> parseIO ["x","y"] "f(x,c)"
-- Fun "f" [Var "x",Fun "c" []]
parseIO :: [String] -> String -> IO (Term String String)
parseIO xs input = case fromString xs input of
  Left err -> do { putStr "parse error at "; print err; mzero }
  Right t  -> return t

-- | @fromString xs s@ parsers a term from the string @s@, where elements of @xs@
-- are considered as variables.
fromString :: [String] -> String -> Either ParseError (Term String String)
fromString xs = runP (parseWST xs) () ""


-- | @parse funP varP@ is a parser for terms, where @funP@ and @varP@ are
-- parsers for function symbols and variables, respectively. The @varP@ parser
-- has a higher priority than the @funP@ parser. Hence, whenever @varP@
-- succeeds, the token is treated as a variable.
-- 
-- Note that the user has to take care of handling trailing white space in
-- @funP@ and @varP@.
parse :: Stream s m Char => ParsecT s u m f -> ParsecT s u m v
  -> ParsecT s u m (Term f v)
parse funP varP = term <?> "term"
  where
    term = try (liftM Var varP) <|> liftM2 Fun funP args
    args = between (lex $ char '(') (lex $ char ')') (sepBy term (lex $ char ','))
             <|> return []


-- | A parser for terms following the conventions of the ancient ASCII input
-- format for the termination competition: every @Char@ that is neither a white
-- space (according to @Data.Char.isSpace@) nor one of @'('@, @')'@, or @','@,
-- is considered a letter. An identifier is a non-empty sequence of letters and
-- it is treated as variable iff it is contained in @xs@.

-- change name?
parseWST :: Stream s m Char => [String] -> ParsecT s u m (Term String String)
parseWST xs = parse (parseFun identWST) (parseVar identWST xs)

-- | @parseFun ident@ parses function symbols defined by @ident@.
parseFun :: Stream s m Char => ParsecT s u m String -> ParsecT s u m String
parseFun id = lex id <?> "function symbol"

-- | @parseVar ident vars@ parses variables as defined by @ident@ and with the
-- additional requirement that the result is a member of @vars@.
parseVar :: Stream s m Char =>
  ParsecT s u m String -> [String] -> ParsecT s u m String
parseVar id xs =
  do { x <- lex id; guard (x `elem` xs); return x }
    <?> "variable"

identWST :: Stream s m Char => ParsecT s u m String
identWST = ident "(),"

-- | @ident tabu@ parses a non-empty sequence of non-space characters not
-- containing elements of @tabu@.
ident :: Stream s m Char => String -> ParsecT s u m String
ident tabu = many1 (satisfy (\c -> not (isSpace c) && not (c `elem` tabu)))

-- Same as @p@ but also consume trailing white space.
lex p = do { x <- p; spaces; return x }
