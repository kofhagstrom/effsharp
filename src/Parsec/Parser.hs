{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Parsec.Parser (Parser (..), (*>>=), bindResult, joinResult, fail, succeed, mapError, choice, end, liftResult) where

import Base.Result (Result (..))
import qualified Base.Result as Result
import Control.Applicative (Alternative (empty, (<|>)), asum)
import Parsec.Error (ParseError (UnexpectedError))
import Stream.Stream (Stream (uncons))
import Prelude hiding (fail)

newtype (Stream s Maybe v) => Parser s v output
  = Parser
  { run ::
      s ->
      Result (s, ParseError v) (s, output)
  }

instance (Semigroup input, Stream input Maybe value) => Functor (Parser input value) where
  fmap f p = Parser $ \input -> do
    (input', x) <- run p input
    return (input', f x)

instance (Semigroup input, Stream input Maybe value) => Applicative (Parser input value) where
  pure output = Parser $ \input -> Ok (input, output)
  p1 <*> p2 = Parser $ \input -> do
    (input', f) <- run p1 input
    (input'', a) <- run p2 input'
    return (input'', f a)

instance (Semigroup input, Monoid input, Stream input Maybe value) => Alternative (Parser input value) where
  empty = Parser $ \_ -> Error (mempty, mempty)
  p1 <|> p2 =
    Parser $
      \input ->
        Result.onError
          (run p1 input)
          ( \(_, e) ->
              Result.onError (run p2 input) (\(rest, e') -> Error (rest, e <> e'))
          )

instance (Semigroup input, Stream input Maybe value) => Monad (Parser input value) where
  return = pure
  p >>= f = Parser $ \input -> do
    (input', a) <- run p input
    run (f a) input'

bindResult :: (Stream s Maybe v) => Parser s v t -> (t -> Result (ParseError v) output) -> Parser s v output
bindResult p f = Parser $
  \input -> do
    (input', token) <- run p input
    case f token of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)

(*>>=) :: (Stream s Maybe v) => Parser s v t -> (t -> Result (ParseError v) o) -> Parser s v o
(*>>=) = bindResult

joinResult :: (Stream s Maybe v) => Parser s v (Result (ParseError v) o) -> Parser s v o
joinResult p = Parser $
  \input -> do
    (input', res) <- run p input
    case res of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)

liftResult :: (Stream s Maybe v) => Result (ParseError v) o -> Parser s v o
liftResult p = Parser $
  \input -> do
    (input', res) <- run (return p) input
    case res of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)

fail :: (Stream s Maybe v) => ParseError v -> Parser s v o
fail e = Parser $ \input -> Error (input, e)

succeed :: (Stream s Maybe v) => o -> Parser s v o
succeed o = Parser $ \input -> Ok (input, o)

mapError :: (Stream t Maybe v) => (t -> ParseError v -> ParseError v) -> Parser t v output -> Parser t v output
mapError f p = Parser $ \input -> Result.mapError (f input <$>) (run p input)

choice :: (Stream s Maybe v) => [Parser s v o] -> Parser s v o
choice = asum

end :: (Stream s Maybe v) => Parser s v ()
end = Parser $ \input -> case uncons input of
  Just _ -> Error (input, UnexpectedError "Expected end of input")
  Nothing -> Ok (input, ())
