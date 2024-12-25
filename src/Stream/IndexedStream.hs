{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Stream.IndexedStream (IndexedStream (..), indexedStreamFromString) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Stream.Stream (Stream, uncons)

newtype Row = Row Int deriving (Eq, Ord)

newtype Col = Col Int deriving (Eq, Ord)

data SourcePosition a = Pos Row Col a deriving (Eq)

newtype IndexedStream value = IndexedStream [SourcePosition value] deriving (Eq)

instance Stream (IndexedStream value) value where
  uncons (IndexedStream []) = Nothing
  uncons (IndexedStream (Pos _ _ a : rest)) = Just (IndexedStream rest, a)

instance Semigroup (IndexedStream value) where
  (<>) (IndexedStream xs) (IndexedStream ys) =
    IndexedStream $ sortBy comparePositions (xs ++ ys)
    where
      comparePositions (Pos row1 col1 _) (Pos row2 col2 _) =
        comparing id (row1, col1) (row2, col2)

instance Monoid (IndexedStream value) where
  mempty = IndexedStream []
  mappend = (<>)

indexedStreamFromString :: String -> IndexedStream Char
indexedStreamFromString str = IndexedStream sourcePositions
  where
    sourcePositions = concatMap processRow $ zip [1 ..] $ lines str
    processRow (rowNum, row) = zipWith (Pos (Row rowNum) . Col) [1 ..] row

instance (Show a) => Show (IndexedStream a) where
  show (IndexedStream (Pos _ _ a : rest)) = show a ++ show (IndexedStream rest)
  show (IndexedStream []) = ""