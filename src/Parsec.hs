{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Parsec
  ( ParseError,
    satisfy,
    while,
    exact,
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
import Result (Result (..), mapError)
import Stream (Stream, consume, uncons)
import Prelude hiding (all)

newtype ParseError = UnexpectedError String

instance Show ParseError where
  show (UnexpectedError msg) = msg ++ "\n"

satisfy :: (Stream input output) => (output -> Bool) -> Parser input [ParseError] output
satisfy cond =
  Parser $ \input -> do
    (rest, value) <- mapError (input,) $ consume [UnexpectedError "Missing input"] input
    if cond value
      then Ok (rest, value)
      else Error (input, [UnexpectedError "Unexpected character"])

exact :: (Eq a, Stream [a] a) => [a] -> Parser [a] [ParseError] [a]
exact = traverse (satisfy . (==))

oneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input [ParseError] output
oneOf these = satisfy (`elem` these)

manyOf :: (Monoid input, Stream input a, Foldable t, Eq a) => t a -> Parser input [ParseError] [a]
manyOf these = many (oneOf these)

noneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input [ParseError] output
noneOf these = satisfy (`notElem` these)

while :: (Stream input a) => (a -> Bool) -> Parser input error [a]
while cond = Parser $ \input ->
  let go acc rest =
        case uncons rest of
          Just (rest', v) | cond v -> go (v : acc) rest'
          _ -> Ok (rest, reverse acc)
   in go [] input

skip :: (Eq a, Stream [a] a) => [a] -> Parser [a] [ParseError] ()
skip this = void $ exact this

ignore :: (Functor f) => f a -> f ()
ignore p = p $> ()

orElse :: (Alternative t) => t a -> t a -> t a
orElse = (<|>)

next :: (Stream s output) => Parser s [ParseError] output
next = Parser $ \input -> mapError (input,) $ consume [UnexpectedError "Missing input"] input

-- parses a grammar of type <A> ::= <B> { ("a" | "b" | ... ) <B> }
-- parserB is a parser which parses Bs, and tokensToOutput is a function which matches input tokens to corresponding outputs
loop :: (Monoid s, Stream s t) => (t -> b -> b -> Maybe b) -> Parser s [ParseError] b -> Parser s [ParseError] b
loop tokensToOutput parseB = parseB >>= loop'
  where
    loop' b =
      ( do
          token <- next
          b' <- parseB
          maybe empty loop' (tokensToOutput token b b')
      )
        `orElse` return b