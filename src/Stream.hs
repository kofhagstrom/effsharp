{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Stream (Stream, uncons, SourcePosition (..), IndexedStream (..), consume, indexedStreamFromString) where

import Result (Result (..), fromMaybe, mapError)

class Stream s v | s -> v where
  uncons :: s -> Maybe (s, v)

consume :: (Stream s v) => e -> s -> Result (s, e) (s, v)
consume err input =
  mapError (input,) . fromMaybe err $ uncons input

newtype Row = Row Int

newtype Col = Col Int

data SourcePosition a = Pos Row Col a

newtype IndexedStream value = IndexedStream [SourcePosition value]

instance Stream (IndexedStream value) value where
  uncons (IndexedStream []) = Nothing
  uncons (IndexedStream (Pos _ _ a : rest)) = Just (IndexedStream rest, a)

instance (Show a) => Show (IndexedStream a) where
  show stream =
    case uncons stream of
      Just (this, rest) -> show this ++ show rest
      Nothing -> ""

indexedStreamFromString :: String -> IndexedStream Char
indexedStreamFromString str = IndexedStream sourcePositions
  where
    sourcePositions = concatMap processRow (zip [1 ..] ls)
    ls = lines str
    processRow (rowNum, row) = zipWith (Pos (Row rowNum) . Col) [1 ..] row