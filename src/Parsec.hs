{-# LANGUAGE FlexibleContexts #-}

module Parsec
  ( ParseError,
    match,
    while,
    parse,
    skip,
    oneOf,
    noneOf,
    ignore,
    orElse,
    manyOf,
    next,
    loop,
  )
where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (void)
import Data.Functor (($>))
import GHC.Base (many)
import Parser (Parser (Parser))
import Result (Result (..))
import Stream (Stream, consume, uncons)
import Prelude hiding (all)

newtype ParseError = UnexpectedError String

instance Show ParseError where
  show (UnexpectedError msg) = msg ++ "\n"

match :: (Stream s output) => (output -> Bool) -> Parser s [ParseError] output
match cond =
  Parser $ \input -> do
    (rest, value) <- consume [UnexpectedError "Missing input"] input
    if cond value
      then Ok (rest, value)
      else Error (input, [UnexpectedError "Unexpected character"])

parse :: (Eq a, Stream [a] a) => [a] -> Parser [a] [ParseError] [a]
parse = traverse (match . (==))

oneOf :: (Stream s output, Eq output) => [output] -> Parser s [ParseError] output
oneOf these = match (`elem` these)

manyOf :: (Stream s output, Eq output, Monoid s) => [output] -> Parser s [ParseError] [output]
manyOf these = many (oneOf these)

noneOf :: (Stream s output, Eq output) => [output] -> Parser s [ParseError] output
noneOf these = match (`notElem` these)

while :: (Stream input a) => (a -> Bool) -> Parser input error [a]
while cond = Parser $ \input ->
  let go acc rest =
        case uncons rest of
          Just (rest', v) | cond v -> go (v : acc) rest'
          _ -> Ok (rest, reverse acc)
   in go [] input

skip :: (Eq a, Stream [a] a) => [a] -> Parser [a] [ParseError] ()
skip this = void $ parse this

ignore :: (Functor f) => f a -> f ()
ignore p = p $> ()

orElse :: (Alternative t) => t a -> t a -> t a
orElse = (<|>)

next :: (Stream s output) => Parser s [ParseError] output
next = Parser $ consume [UnexpectedError "Missing input"]

-- parses a grammar of type <A> ::= <B> { ("a" | "b" | ... ) <B> }
-- parserB is a parser which parses Bs, and tokensToOutput is a function which matches input tokens to corresponding outputs
loop :: (Monoid s, Stream s t) => (t -> b -> b -> Maybe b) -> Parser s [ParseError] b -> Parser s [ParseError] b
loop tokensToOutput parserB = parserB >>= loop'
  where
    loop' e =
      ( do
          t <- next
          p <- parserB
          maybe empty loop' (tokensToOutput t e p)
      )
        `orElse` return e