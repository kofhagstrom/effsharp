{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IndexedStream (IndexedStream (..), indexedStreamFromString) where

import Stream (Stream, uncons)

newtype Row = Row Int deriving (Eq)

newtype Col = Col Int deriving (Eq)

data SourcePosition a = Pos Row Col a deriving (Eq)

newtype IndexedStream value = IndexedStream [SourcePosition value] deriving (Eq)

instance Stream (IndexedStream value) value where
  uncons (IndexedStream []) = Nothing
  uncons (IndexedStream (Pos _ _ a : rest)) = Just (IndexedStream rest, a)

instance Semigroup (IndexedStream value) where
  (<>) (IndexedStream xs) (IndexedStream ys) = IndexedStream (xs ++ ys)

instance Monoid (IndexedStream value) where
  mempty = IndexedStream []
  mappend = (<>)

indexedStreamFromString :: String -> IndexedStream Char
indexedStreamFromString str = IndexedStream sourcePositions
  where
    sourcePositions = concatMap processRow (zip [1 ..] ls)
    ls = lines str
    processRow (rowNum, row) = zipWith (Pos (Row rowNum) . Col) [1 ..] row

showIndexedStream :: (Show a) => IndexedStream a -> String
showIndexedStream (IndexedStream (Pos _ _ a : rest)) = show a ++ showIndexedStream (IndexedStream rest)
showIndexedStream (IndexedStream []) = ""

instance (Show a) => Show (IndexedStream a) where
  show = showIndexedStream