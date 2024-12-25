module Parsec.Error (ParseError (..)) where

data ParseError = MissingInput | UnexpectedToken | UnexpectedError String deriving (Eq)

instance Show ParseError where
  show (UnexpectedError msg) = msg ++ "\n"
  show MissingInput = "Missing input"
  show UnexpectedToken = "Unexpected token"
