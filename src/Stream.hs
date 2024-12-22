{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Stream (Stream, uncons, SourcePosition (..), IndexedStream (..), consume) where

import Result (Result (..), fromMaybe, mapError)

class Stream s v | s -> v where
  uncons :: s -> Maybe (s, v)

data SourcePosition = Pos Int Int

newtype IndexedStream value = IndexedStream [(SourcePosition, value)]

instance Stream (IndexedStream value) value where
  uncons (IndexedStream []) = Nothing
  uncons (IndexedStream ((_, a) : rest)) = Just (IndexedStream rest, a)

consume :: (Stream s v) => e -> s -> Result (s, e) (s, v)
consume e input =
  mapError (input,) (fromMaybe e (uncons input))
