module Parsec.Error (ParseError (..)) where

data ParseError = MissingInput | UnexpectedToken | UnexpectedError String | MultipleError [ParseError] deriving (Eq)

instance Show ParseError where
  show (UnexpectedError msg) = "Unexpected error: " ++ msg
  show MissingInput = "Missing input"
  show UnexpectedToken = "Unexpected token"
  show (MultipleError errors) = foldr (\e acc -> show e ++ ", " ++ acc) "" errors
