{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
module Parser (Parser (..), (>>>=)) where

import Control.Applicative (Alternative (empty, (<|>)))
import Result (Result (..), onError)

newtype Parser input error output
  = Parser
  { run ::
      input ->
      Result (input, error) (input, output)
  }

instance (Semigroup input) => Functor (Parser input error) where
  fmap f p = Parser $ \input -> do
    (input', x) <- run p input
    return (input', f x)

instance (Semigroup input) => Applicative (Parser input error) where
  pure output = Parser $ \input -> Ok (input, output)
  p1 <*> p2 = Parser $ \input -> do
    (input', f) <- run p1 input
    (input'', a) <- run p2 input'
    return (input'', f a)

instance (Semigroup input, Monoid input, Monoid error) => Alternative (Parser input error) where
  empty = Parser $ \_ -> Error (mempty, mempty)
  p1 <|> p2 =
    Parser $
      \input ->
        onError
          (run p1 input)
          ( \(e, _) ->
              onError (run p2 input) (\(e', rest) -> Error (e <> e', rest))
          )

instance (Semigroup input) => Monad (Parser input error) where
  return = pure
  p >>= f = Parser $ \input -> do
    (input', a) <- run p input
    run (f a) input'

bindError :: Parser input error t -> (t -> Result error output) -> Parser input error output
bindError p f = Parser $
  \input -> do
    (input', token) <- run p input
    case f token of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)

(>>>=) :: Parser input error t -> (t -> Result error output) -> Parser input error output
(>>>=) = bindError