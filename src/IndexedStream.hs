{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IndexedStream (IndexedStream (..), indexedStreamFromString) where

import Stream (Stream, uncons)

newtype Row = Row Int

newtype Col = Col Int

data SourcePosition a = Pos Row Col a

newtype IndexedStream value = IndexedStream [SourcePosition value]

instance Stream (IndexedStream value) value where
  uncons (IndexedStream []) = Nothing
  uncons (IndexedStream (Pos _ _ a : rest)) = Just (IndexedStream rest, a)

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