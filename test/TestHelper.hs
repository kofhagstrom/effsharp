module TestHelper (testRun, (|>)) where

import Base.Result (Result, mapError)
import Parsec.Error (ParseError)
import Parsec.Parser (Parser (run))

testRun :: Parser a ok -> a -> Result [ParseError] ok
testRun p input = mapError snd (snd <$> run p input)

pipe :: a -> (a -> b) -> b
pipe x f = f x

(|>) :: a -> (a -> b) -> b
(|>) = pipe