module Result (Result (..), fromMaybe, mapError) where

data Result err ok = Ok ok | Error err

instance Functor (Result err) where
  fmap f (Ok ok) = Ok (f ok)
  fmap _ (Error err) = Error err

instance Applicative (Result err) where
  pure = Ok
  fRes <*> res =
    case fRes of
      Ok f ->
        f <$> res
      Error err -> Error err

instance Monad (Result err) where
  return = pure
  res >>= f =
    case res of
      Ok ok -> f ok
      Error err -> Error err

fromMaybe :: err -> Maybe ok -> Result err ok
fromMaybe err Nothing = Error err
fromMaybe _ (Just ok) = Ok ok

mapError :: (err1 -> err2) -> Result err1 ok -> Result err2 ok
mapError f (Error err) = Error $ f err
mapError _ (Ok ok) = Ok ok