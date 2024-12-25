{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsec.Parser (Parser (..), (*>>=)) where

import Base.Result (Result (..), onError)
import Control.Applicative (Alternative (empty, (<|>)))
import Parsec.Error (ParseError)

newtype Parser input output
  = Parser
  { run ::
      input ->
      Result (input, [ParseError]) (input, output)
  }

instance (Semigroup input) => Functor (Parser input) where
  fmap f p = Parser $ \input -> do
    (input', x) <- run p input
    return (input', f x)

instance (Semigroup input) => Applicative (Parser input) where
  pure output = Parser $ \input -> Ok (input, output)
  p1 <*> p2 = Parser $ \input -> do
    (input', f) <- run p1 input
    (input'', a) <- run p2 input'
    return (input'', f a)

instance (Semigroup input, Monoid input) => Alternative (Parser input) where
  empty = Parser $ \_ -> Error (mempty, mempty)
  p1 <|> p2 =
    Parser $
      \input ->
        onError
          (run p1 input)
          ( \(e, _) ->
              onError (run p2 input) (\(e', rest) -> Error (e <> e', rest))
          )

instance (Semigroup input) => Monad (Parser input) where
  return = pure
  p >>= f = Parser $ \input -> do
    (input', a) <- run p input
    run (f a) input'

bindError :: Parser input t -> (t -> Result [ParseError] output) -> Parser input output
bindError p f = Parser $
  \input -> do
    (input', token) <- run p input
    case f token of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)

(*>>=) :: Parser input t -> (t -> Result [ParseError] output) -> Parser input output
(*>>=) = bindError