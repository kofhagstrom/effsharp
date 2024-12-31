{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Parsec.Parser (Parser (..), (*>>=), bindResult, joinResult, fail, succeed, mapError, choice, end, liftResult) where

import Base.Result (Result (..))
import qualified Base.Result as Result
import Control.Applicative (Alternative (empty, (<|>)), asum)
import Control.Monad (MonadPlus, mplus, mzero)
import Parsec.Error (ParseError (UnexpectedError))
import Stream.Stream (Stream (uncons))
import Prelude hiding (fail)

newtype (Stream s Maybe v, Monad m) => Parser s v m output
  = Parser
  { run ::
      s ->
      m (Result (s, ParseError v) (s, output))
  }

instance (Semigroup input, Stream input Maybe value, Monad m) => Functor (Parser input value m) where
  fmap f p = Parser $ \input -> do
    result <- run p input
    return $ case result of
      Ok (input', x) -> Ok (input', f x)
      Error e -> Error e

instance (Semigroup input, Stream input Maybe value, Monad m) => Applicative (Parser input value m) where
  pure output = Parser $ \input -> return $ Ok (input, output)
  p1 <*> p2 = Parser $ \input -> do
    result1 <- run p1 input
    case result1 of
      Ok (input', f) -> do
        result2 <- run p2 input'
        return $ case result2 of
          Ok (input'', a) -> Ok (input'', f a)
          Error e -> Error e
      Error e -> return $ Error e

instance (Semigroup input, Monoid input, Stream input Maybe value, Monad m) => Alternative (Parser input value m) where
  empty = Parser $ \_ -> return $ Error (mempty, mempty)
  p1 <|> p2 = Parser $ \input -> do
    result1 <- run p1 input
    case result1 of
      Ok ok -> return $ Ok ok
      Error e1 -> do
        result2 <- run p2 input
        return $ case result2 of
          Ok ok -> Ok ok
          Error e2 -> Error (input, snd e1 <> snd e2)

instance (Semigroup input, Stream input Maybe value, Monad m) => Monad (Parser input value m) where
  return = pure
  p >>= f = Parser $ \input -> do
    result <- run p input
    case result of
      Ok (input', a) -> run (f a) input'
      Error e -> return $ Error e

instance (Semigroup input, Monoid input, Stream input Maybe value, Monad m) => MonadPlus (Parser input value m) where
  mzero = empty
  mplus = (<|>)

bindResult :: (Stream s Maybe v, Monad m) => Parser s v m t -> (t -> Result (ParseError v) output) -> Parser s v m output
bindResult p f = Parser $ \input -> do
  result <- run p input
  return $ case result of
    Ok (input', token) -> case f token of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)
    Error e -> Error e

(*>>=) :: (Stream s Maybe v, Monad m) => Parser s v m t -> (t -> Result (ParseError v) o) -> Parser s v m o
(*>>=) = bindResult

joinResult :: (Stream s Maybe v, Monad m) => Parser s v m (Result (ParseError v) o) -> Parser s v m o
joinResult p = Parser $ \input -> do
  result <- run p input
  return $ case result of
    Ok (input', res) -> case res of
      Ok ok -> Ok (input', ok)
      Error e -> Error (input, e)
    Error e -> Error e

liftResult :: (Stream s Maybe v, Monad m) => Result (ParseError v) o -> Parser s v m o
liftResult p = Parser $ \input -> return $ case p of
  Ok ok -> Ok (input, ok)
  Error e -> Error (input, e)

fail :: (Stream s Maybe v, Monad m) => ParseError v -> Parser s v m o
fail e = Parser $ \input -> return $ Error (input, e)

succeed :: (Stream s Maybe v, Monad m) => o -> Parser s v m o
succeed o = Parser $ \input -> return $ Ok (input, o)

mapError :: (Stream t Maybe v, Monad m) => (t -> ParseError v -> ParseError v) -> Parser t v m output -> Parser t v m output
mapError f p = Parser $ \input -> do
  result <- run p input
  return $ Result.mapError (f input <$>) result

choice :: (Stream s Maybe v, Monad m) => [Parser s v m o] -> Parser s v m o
choice = asum

end :: (Stream s Maybe v, Monad m) => Parser s v m ()
end = Parser $ \input -> return $ case uncons input of
  Just _ -> Error (input, UnexpectedError "Expected end of input")
  Nothing -> Ok (input, ())
