module Result (Result (..), fromMaybe, mapError) where

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

fromMaybe :: e -> Maybe a -> Result e a
fromMaybe e Nothing = Error e
fromMaybe _ (Just a) = Ok a

mapError :: (e1 -> e2) -> Result e1 a -> Result e2 a
mapError f (Error e) = Error $ f e
mapError _ (Ok a) = Ok a