module TestHelper (testRun, (|>)) where

import Base.Result (Result, mapError)
import Parsec.Error (ParseError)
import Parsec.Parser (Parser (run))
import Stream.Stream

testRun :: (Stream a v) => Parser a v ok -> a -> Result (ParseError v) ok
testRun p input = mapError snd (snd <$> run p input)

pipe :: a -> (a -> b) -> b
pipe x f = f x

(|>) :: a -> (a -> b) -> b
(|>) = pipe