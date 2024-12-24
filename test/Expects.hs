module Expects where

import Parser (Parser (..))
import Result
import ResultHelper (unwrapError)

ok :: Parser a error b -> a -> Result (a, error) b
ok f input = snd <$> run f input

error :: (Show ok) => Result (a, b) ok -> b
error runner = snd $ unwrapError runner