{-# LANGUAGE FlexibleContexts #-}

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

import Base.Result (Result (..))
import Control.Applicative (Alternative (some), (<|>))
import Control.Monad (void)
import Parsec.Error (ParseError (..))
import Parsec.Parser (Parser (Parser, run), bindResult, (*>>=))
import Stream.Stream (Stream, consume, uncons)
import Prelude hiding (all, or)

while :: (Stream input Maybe value, Monad m) => (value -> Bool) -> Parser input value m [value]
while cond = Parser $ \input ->
  let consumer acc rest =
        case uncons rest of
          Just (rest', v) | cond v -> consumer (v : acc) rest'
          _ -> return $ Ok (rest, reverse acc)
   in consumer [] input

satisfy :: (Stream input Maybe output, Monad m) => (output -> Bool) -> Parser input output m output
satisfy cond =
  Parser $ \input -> do
    let result = consume [MissingInput] input
    return $ case result of
      Ok (rest, value) ->
        if cond value
          then Ok (rest, value)
          else Error (input, UnexpectedToken value)
      Error e -> Error (input, head e)

equal :: (Eq output, Stream input Maybe output, Monad m) => output -> Parser input output m output
equal this = satisfy (this ==)

notEqual :: (Eq output, Stream input Maybe output, Monad m) => output -> Parser input output m output
notEqual this = satisfy (/= this)

exact :: (Eq value, Stream input Maybe value, Semigroup input, Monad m) => [value] -> Parser input value m [value]
exact = traverse equal

oneOf :: (Stream input Maybe output, Foldable t, Eq output, Monad m) => t output -> Parser input output m output
oneOf these = satisfy (`elem` these)

someOf :: (Monoid input, Stream input Maybe value, Foldable t, Eq value, Monad m) => t value -> Parser input value m [value]
someOf these = some (oneOf these)

noneOf :: (Stream input Maybe output, Foldable t, Eq output, Monad m) => t output -> Parser input output m output
noneOf these = satisfy (`notElem` these)

skip :: (Eq value, Stream input Maybe value, Semigroup input, Monad m) => [value] -> Parser input value m ()
skip = void . exact

next :: (Stream s Maybe t, Monad m) => Parser s t m t
next = Parser $ \input ->
  case consume [MissingInput] input of
    Ok (rest, value) -> return $ Ok (rest, value)
    Error e -> return $ Error (input, head e)

loop :: (Monad m, Alternative m) => m a -> m [a]
loop p = do
  x <- p
  xs <- loop p <|> return []
  return (x : xs)

or :: (Alternative f) => f a -> f a -> f a
or = (<|>)

ignore :: (Stream input Maybe value, Monad m) => Parser input value m output -> Parser input value m ()
ignore = void