module Base.SourcePosition (SourcePosition (..), Row (..), Col (..), mkPos) where

newtype Row = Row Int deriving (Eq, Ord)

newtype Col = Col Int deriving (Eq, Ord)

data SourcePosition a = Pos Row Col a deriving (Eq)

mkPos :: Int -> Int -> a -> SourcePosition a
mkPos r c = Pos (Row r) (Col c)

instance (Show a) => Show (SourcePosition a) where
  show (Pos (Row row) (Col col) a) = show row ++ ":" ++ show col ++ " " ++ show a