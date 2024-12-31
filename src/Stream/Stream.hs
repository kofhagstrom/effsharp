{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Stream.Stream (Stream, uncons, consume, values) where

import Base.Result (Result)
import qualified Base.Result as Result

class (Monoid s, Monad m) => Stream s m v | s -> v where
  uncons :: s -> m (s, v)

consume :: (Stream s Maybe v) => err -> s -> Result err (s, v)
consume err input =
  Result.fromMaybe err $ uncons input

values :: (Stream s Maybe v) => s -> [v]
values input =
  case uncons input of
    Just (rest, v) -> v : values rest
    Nothing -> []
