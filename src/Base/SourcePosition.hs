module Base.SourcePosition (SourcePosition (..), Line (..), Col (..), mkPos) where

newtype Line = Line Int deriving (Eq, Ord)

newtype Col = Col Int deriving (Eq, Ord)

data SourcePosition a = Pos Line Col a deriving (Eq)

mkPos :: Int -> Int -> a -> SourcePosition a
mkPos r c = Pos (Line r) (Col c)

instance (Show a) => Show (SourcePosition a) where
  show (Pos (Line line) (Col col) a) = "(" ++ show line ++ "," ++ show col ++ "): " ++ show a