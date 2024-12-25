module Parsec.Error (ParseError (..)) where

data ParseError t = MissingInput | UnexpectedToken t | UnexpectedError String | MultipleError [ParseError t] deriving (Eq)

instance (Show t) => Show (ParseError t) where
  show (UnexpectedError msg) = "Unexpected error: " ++ msg
  show MissingInput = "Missing input"
  show (UnexpectedToken t) = "Unexpected token " ++ show t
  show (MultipleError errors) = foldr (\e acc -> show e ++ ", " ++ acc) "" errors
