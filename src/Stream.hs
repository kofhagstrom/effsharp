{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Stream (Stream, uncons, SourcePosition (..), IndexedStream (..), consume) where

import Result (Result (..), fromOption)

class Stream s v | s -> v where
  uncons :: s -> Maybe (s, v)

data SourcePosition = Pos Int Int

newtype IndexedStream value = IndexedStream [(SourcePosition, value)]

instance Stream (IndexedStream value) value where
  uncons (IndexedStream []) = Nothing
  uncons (IndexedStream ((_, a) : rest)) = Just (IndexedStream rest, a)

consume :: Stream s v => e -> s -> Result e (s, v)
consume e input = fromOption e (uncons input)