{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parsec
  ( parseMatch,
  -- parseWhile,
  -- parse,
  -- skip,
  -- oneOf,
  -- noneOf,
  -- ignore,
  -- orElse,
  -- manyOf,
  -- next,
  -- loop,
  )
where

import Parser (Parser (Parser))
import Result (Result (..))
import Stream (Stream, consume)
import Prelude hiding (all)

newtype ParseError = UnexpectedError String

instance Show ParseError where
  show (UnexpectedError msg) = msg ++ "\n"

consumer :: (Stream s v) => s -> Result (s, [ParseError]) (s, v)
consumer = consume [UnexpectedError "Missing input"]

parseMatch :: (Stream s output) => (output -> Bool) -> Parser s [ParseError] output
parseMatch match =
  Parser $ \input -> do
    (rest, value) <- consumer input
    if match value
      then Ok (rest, value)
      else Error (input, [UnexpectedError "Unexpected character"])

of_ :: (Stream a1 b1, Foldable t) => (b1 -> (t a2, a1)) -> Parser a1 [ParseError] (t a2)
of_ f =
  Parser $ \input ->
    case consumer input of
      Error (remainingInput, errors) -> Error (remainingInput, errors)
      Ok (_, rest) ->
        let (parsed, newRest) = f rest
         in if null parsed
              then Error (newRest, [UnexpectedError "Empty result"])
              else Ok (newRest, parsed)

-- oneOf options = of_ f
-- where
--   f input = case input of
--     (x : xs) -> if x `elem` options then ([x], xs) else ([], input)
--     [] -> ([], [])
--
-- manyOf :: (Eq a) => [a] -> Parser [a] [a]
-- manyOf options = of_ . span $ (`elem` options)
--
-- noneOf :: (Eq t) => [t] -> Parser [t] [t]
-- noneOf options = of_ . break $ (`elem` options)
--
-- parseWhile :: (a -> Bool) -> Parser [a] [a]
-- parseWhile f = Parser $ \input ->
--  let (str, rest) = span f input
--   in Right (str, rest)
--
-- parse :: (Eq a) => [a] -> Parser [a] [a]
-- parse = traverse (parseMatch . (==))
--
-- skip :: (Eq a) => [a] -> Parser [a] ()
-- skip c = () <$ parse c
--
-- ignore :: (Functor f) => f a -> f ()
-- ignore p = p $> ()
--
-- orElse :: (Alternative t) => t a -> t a -> t a
-- orElse = (<|>)
--
-- next :: Parser [o] o
-- next = Parser $ \case
--  (t : ts) -> Right (t, ts)
--  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])
--
---- parses a grammar of type <A> ::= <B> { ("a" | "b" | ... ) <B> }
---- parserB is a parser which parses Bs, and tokensToOutput is a function which matches input tokens to corresponding outputs
-- loop :: (t1 -> t2 -> t2 -> Maybe t2) -> Parser [t1] t2 -> Parser [t1] t2
-- loop tokensToOutput parserB = parserB >>= loop'
--  where
--    loop' e =
--      ( do
--          t <- next
--          p <- parserB
--          maybe empty loop' (tokensToOutput t e p)
--      )
--        `orElse` return e