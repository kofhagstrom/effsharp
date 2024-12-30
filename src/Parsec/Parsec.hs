{-# LANGUAGE TupleSections #-}

module Parsec.Parsec
  ( Parser (run),
    (*>>=),
    bindResult,
    ParseError (..),
    satisfy,
    while,
    exact,
    skip,
    oneOf,
    noneOf,
    someOf,
    next,
    loop,
    or,
    equal,
    notEqual,
    ignore,
  )
where

import Base.Result (Result (..), mapError)
import Control.Applicative (Alternative (some), (<|>))
import Control.Monad (void)
import Parsec.Error (ParseError (..))
import Parsec.Parser (Parser (Parser, run), bindResult, (*>>=))
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

equal :: (Eq output, Stream input output) => output -> Parser input output output
equal this = satisfy (this ==)

notEqual :: (Eq output, Stream input output) => output -> Parser input output output
notEqual this = satisfy (/= this)

exact :: (Eq value, Stream input value, Semigroup input) => [value] -> Parser input value [value]
exact = traverse equal

oneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input output output
oneOf these = satisfy (`elem` these)

someOf :: (Monoid input, Stream input value, Foldable t, Eq value) => t value -> Parser input value [value]
someOf these = some (oneOf these)

noneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input output output
noneOf these = satisfy (`notElem` these)

skip :: (Eq value, Stream input value, Semigroup input) => [value] -> Parser input value ()
skip = void . exact

next :: (Stream s t) => Parser s t t
next = Parser $ \input -> mapError (input,) $ consume MissingInput input

loop :: (Monad m, Alternative m) => m a -> m [a]
loop p = do
  x <- p
  xs <- loop p <|> return []
  return (x : xs)

or :: (Alternative f) => f a -> f a -> f a
or = (<|>)

ignore :: (Stream input value) => Parser input value output -> Parser input value ()
ignore = void
