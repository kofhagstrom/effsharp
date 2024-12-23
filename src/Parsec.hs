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

of_ :: (Stream input v, Foldable t) => (v -> (input, t a)) -> Parser input [ParseError] (t a)
of_ f =
  Parser $ \input -> do
    (_, value) <- consume [UnexpectedError "Missing input"] input
    let (rest, value') = f value
     in if null value'
          then Error (rest, [UnexpectedError "Empty result"])
          else Ok (rest, value')

oneOf :: (Stream [a] [a], Foldable t, Eq a) => t a -> Parser [a] [ParseError] [a]
oneOf these = of_ f
  where
    f input = case input of
      (x : xs) -> if x `elem` these then ([x], xs) else ([], input)
      [] -> ([], [])

manyOf :: (Stream [a] [a], Foldable t, Eq a) => t a -> Parser [a] [ParseError] [a]
manyOf these = of_ . span $ (`elem` these)

noneOf :: (Stream [a] [a], Foldable t, Eq a) => t a -> Parser [a] [ParseError] [a]
noneOf these = of_ . break $ (`elem` these)

while :: (Stream input a) => (a -> Bool) -> Parser input error [a]
while cond = Parser $ \input ->
  let go acc rest =
        case uncons rest of
          Just (rest', v) | cond v -> go (v : acc) rest'
          _ -> Ok (rest, reverse acc)
   in go [] input

parse :: (Eq a, Stream [a] a) => [a] -> Parser [a] [ParseError] [a]
parse = traverse (match . (==))

skip :: (Eq a, Stream [a] a) => [a] -> Parser [a] [ParseError] ()
skip this = void $ parse this

ignore :: (Functor f) => f a -> f ()
ignore parser = parser $> ()

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