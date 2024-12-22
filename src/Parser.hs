module Parser (Parser (..)) where

import Control.Applicative (Alternative (empty, (<|>)))
import Result (Result (..))

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
  p1 <|> p2 = Parser $ \input ->
    case run p1 input of
      Ok a -> Ok a
      Error (e, _) -> case run p2 input of
        Ok a' -> Ok a'
        Error (e', ts') -> Error (e <> e', ts')

instance (Semigroup input) => Monad (Parser input error) where
  return = pure
  p >>= f = Parser $ \input -> do
    (input', a) <- run p input
    run (f a) input'
