module Parsec.Error (ParseError (..)) where

data ParseError t = MissingInput | UnexpectedToken t | UnexpectedError String | MultipleError [ParseError t] deriving (Eq, Show)

instance Semigroup (ParseError t) where
  (<>) (MultipleError es1) (MultipleError es2) = MultipleError (es1 ++ es2)
  (<>) (MultipleError es) e = MultipleError (es ++ [e])
  (<>) e (MultipleError es) = MultipleError (e : es)
  (<>) e1 e2 = MultipleError [e1, e2]

instance Monoid (ParseError t) where
  mempty = UnexpectedError "Empty Error"
