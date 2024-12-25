module Parsec.Error (ParseError (..)) where

data ParseError t = MissingInput | UnexpectedToken t | UnexpectedError String deriving (Eq)

instance (Show t) => Show (ParseError t) where
  show (UnexpectedError msg) = msg ++ "\n"
  show MissingInput = "Missing input"
  show (UnexpectedToken t) = "Unexpected token: " ++ show t
