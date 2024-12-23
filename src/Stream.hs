{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Stream (Stream, uncons, consume) where

import Result (Result (..), fromMaybe)

class Stream s v | s -> v where
  uncons :: s -> Maybe (s, v)

consume :: (Stream s v) => err -> s -> Result err (s, v)
consume err input =
  fromMaybe err $ uncons input
