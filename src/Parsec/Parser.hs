module Parsec.Parser (Parser (..), (*>>=)) where

import Base.Result (Result (..), onError)
import Control.Applicative (Alternative (empty, (<|>)))
import Parsec.Error (ParseError (MultipleError, UnexpectedError))
import Stream.Stream (Stream)

newtype (Stream s v) => Parser s v output
  = Parser
  { run ::
      s ->
      Result (s, ParseError v) (s, output)
  }

instance (Semigroup input, Stream input value) => Functor (Parser input value) where
  fmap f p = Parser $ \input -> do
    (input', x) <- run p input
    return (input', f x)

instance (Semigroup input, Stream input value) => Applicative (Parser input value) where
  pure output = Parser $ \input -> Ok (input, output)
  p1 <*> p2 = Parser $ \input -> do
    (input', f) <- run p1 input
    (input'', a) <- run p2 input'
    return (input'', f a)

instance (Semigroup input, Monoid input, Stream input value) => Alternative (Parser input value) where
  empty = Parser $ \_ -> Error (mempty, UnexpectedError "Unexpected Error")
  p1 <|> p2 =
    Parser $
      \input ->
        onError
          (run p1 input)
          ( \(_, e) ->
              onError (run p2 input) (\(rest, e') -> Error (rest, MultipleError [e, e']))
          )

instance (Semigroup input, Stream input value) => Monad (Parser input value) where
  return = pure
  p >>= f = Parser $ \input -> do
    (input', a) <- run p input
    run (f a) input'

bindError :: (Stream s v) => Parser s v t -> (t -> Result (ParseError v) o) -> Parser s v o
bindError p f = Parser $
  \input -> do
    (input', token) <- run p input
    case f token of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)

(*>>=) :: (Stream s v) => Parser s v t -> (t -> Result (ParseError v) o) -> Parser s v o
(*>>=) = bindError