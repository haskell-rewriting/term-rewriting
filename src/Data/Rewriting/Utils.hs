{-# OPTIONS_GHC -XFlexibleContexts#-}
module Data.Rewriting.Utils (
  parseFromString
) where

import Text.Parsec

parseFromString :: Parsec String () a -> String -> Either ParseError a
parseFromString p = parse (all p) ""
  where all = between spaces (spaces >> eof)
