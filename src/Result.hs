module Result (Result (..), fromOption) where

data Result e a = Ok a | Error e

instance Functor (Result e) where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Error e) = Error e

instance Applicative (Result e) where
  pure = Ok
  res1 <*> res2 =
    case res1 of
      Ok f ->
        f <$> res2
      Error e -> Error e

instance Monad (Result e) where
  return = pure
  res >>= f =
    case res of
      Ok a -> f a
      Error e -> Error e

fromOption :: e -> Maybe a -> Result e a
fromOption e Nothing = Error e
fromOption _ (Just a) = Ok a