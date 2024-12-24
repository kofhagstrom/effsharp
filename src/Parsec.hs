{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Parsec
  ( ParseError (..),
    satisfy,
    while,
    exact,
    skip,
    oneOf,
    noneOf,
    manyOf,
    next,
    loop,
  )
where

import Control.Applicative (Alternative (some), empty, (<|>))
import Control.Monad (void)
import Parser (Parser (Parser))
import Result (Result (..), mapError)
import Stream (Stream, consume, uncons)
import Prelude hiding (all)

data ParseError t = MissingInput | UnexpectedToken t | UnexpectedError String deriving (Eq)

instance (Show t) => Show (ParseError t) where
  show (UnexpectedError msg) = msg ++ "\n"
  show MissingInput = "Missing input"
  show (UnexpectedToken t) = "Unexpected token: " ++ show t

satisfy :: (Stream input output) => (output -> Bool) -> Parser input [ParseError output] output
satisfy cond =
  Parser $ \input -> do
    (rest, value) <- mapError (input,) $ consume [MissingInput] input
    if cond value
      then Ok (rest, value)
      else Error (input, [UnexpectedToken value])

exact :: (Eq a, Stream input a, Semigroup input) => [a] -> Parser input [ParseError a] [a]
exact = traverse (satisfy . (==))

oneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input [ParseError output] output
oneOf these = satisfy (`elem` these)

manyOf :: (Monoid input, Stream input a, Foldable t, Eq a) => t a -> Parser input [ParseError a] [a]
manyOf these = some (oneOf these)

noneOf :: (Stream input output, Foldable t, Eq output) => t output -> Parser input [ParseError output] output
noneOf these = satisfy (`notElem` these)

while :: (Stream input a) => (a -> Bool) -> Parser input error [a]
while cond = Parser $ \input ->
  let consumer acc rest =
        case uncons rest of
          Just (rest', v) | cond v -> consumer (v : acc) rest'
          _ -> Ok (rest, reverse acc)
   in consumer [] input

skip :: (Eq a, Stream input a, Semigroup input) => [a] -> Parser input [ParseError a] ()
skip = void . exact

next :: (Stream s output) => Parser s [ParseError s] output
next = Parser $ \input -> mapError (input,) $ consume [MissingInput] input

-- parses a grammar of type <A> ::= <B> { ("a" | "b" | ... ) <B> }
-- parserB is a parser which parses Bs, and tokensToOutput is a function which matches input tokens to corresponding outputs
loop :: (Monoid s, Stream s t) => (t -> b -> b -> Maybe b) -> Parser s [ParseError s] b -> Parser s [ParseError s] b
loop tokensToOutput parseB = parseB >>= loop'
  where
    loop' b =
      ( do
          token <- next
          b' <- parseB
          maybe empty loop' (tokensToOutput token b b')
      )
        <|> return b
