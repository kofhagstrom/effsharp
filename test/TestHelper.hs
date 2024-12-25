module TestHelper (testRun) where

import Base.Result (Result, mapError)
import Parsec.Parser (Parser (run))

testRun :: Parser a err2 ok -> a -> Result err2 ok
testRun p input = mapError snd (snd <$> run p input)
