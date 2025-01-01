{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Stream.IndexedStream (IndexedStream (..), currPos, Line (..), Col (..), fromString) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Stream.SourcePosition (Col (..), Line (..), SourcePosition (..))
import Stream.Stream (Stream, uncons, values)

newtype IndexedStream value = IndexedStream [SourcePosition value] deriving (Eq)

instance Stream (IndexedStream value) Maybe value where
  uncons (IndexedStream []) = Nothing
  uncons (IndexedStream (Pos _ _ value : rest)) = Just (IndexedStream rest, value)

instance Semigroup (IndexedStream value) where
  (<>) (IndexedStream xs) (IndexedStream ys) =
    IndexedStream $ sortBy comparePositions (xs ++ ys)
    where
      comparePositions (Pos row1 col1 _) (Pos row2 col2 _) =
        comparing id (row1, col1) (row2, col2)

instance Monoid (IndexedStream value) where
  mempty = IndexedStream []
  mappend = (<>)

instance (Show a) => Show (IndexedStream a) where
  show = show . values

fromString :: String -> IndexedStream Char
fromString str = IndexedStream sourcePositions
  where
    sourcePositions = concatMap processRow $ zip [1 ..] $ addNewLines (lines str)
    processRow (rowNum, row) = zipWith (Pos (Line rowNum) . Col) [1 ..] row
    addNewLines ls =
      case ls of
        [] -> []
        [l] -> [l]
        _ -> (++ "\n") <$> init ls ++ [last ls]

currPos :: IndexedStream a -> Maybe (SourcePosition a)
currPos (IndexedStream (pos : _)) = Just pos
currPos _ = Nothing