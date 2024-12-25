{-# LANGUAGE TupleSections #-}

module Parsec.Parsec
  ( Parser (run),
    (*>>=),
    ParseError (..),
    satisfy,
    while,
    exact,
    skip,
    oneOf,
    noneOf,
    manyOf,
    next,
    loop,
    or,
    condition,
  )
where

import Base.Result (Result (..), mapError)
import Control.Applicative (Alternative (some), (<|>))
import Control.Monad (void)
import Parsec.Error (ParseError (..))
import Parsec.Parser (Parser (Parser, run), (*>>=))
import Stream.Stream (Stream, consume, uncons)
import Prelude hiding (all, or)

while :: (Stream input value) => (value -> Bool) -> Parser input value [value]
while cond = Parser $ \input ->
  let consumer acc rest =
        case uncons rest of
          Just (rest', v) | cond v -> consumer (v : acc) rest'
          _ -> Ok (rest, reverse acc)
   in consumer [] input

satisfy :: (Stream input output) => (output -> Bool) -> Parser input output output
satisfy cond =
  Parser $ \input -> do
    (rest, value) <- mapError (input,) $ consume MissingInput input
    if cond value
      then Ok (rest, value)
      else Error (input, UnexpectedToken value)

condition :: (Eq output, Stream input output) => output -> Parser input output output
condition this = satisfy (this ==)

exact :: (Eq value, Stream input value, Semigroup input) => [value] -> Parser input value [value]
exact = traverse condition

oneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input output output
oneOf these = satisfy (`elem` these)

manyOf :: (Monoid input, Stream input value, Foldable t, Eq value) => t value -> Parser input value [value]
manyOf these = some (oneOf these)

noneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input output output
noneOf these = satisfy (`notElem` these)

skip :: (Eq value, Stream input value, Semigroup input) => [value] -> Parser input value ()
skip = void . exact

next :: (Stream s t) => Parser s t t
next = Parser $ \input -> mapError (input,) $ consume MissingInput input

-- parses a grammar of type <A> ::= <B> { <sep> <B> }
loop :: (Alternative m, Monad m) => m a -> m a -> m [a]
loop value sep =
  do
    b <- value
    loop' b
  where
    loop' b =
      ( do
          s <- sep
          anotherOne <- value
          rest <- loop' anotherOne
          return (b : s : rest)
      )
        <|> return [b]

or :: (Alternative f) => f a -> f a -> f a
or = (<|>)
