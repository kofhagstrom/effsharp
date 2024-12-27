{-# LANGUAGE LambdaCase #-}

module TestHelper (testRun, (|>)) where

import Base.Result (Result (Error, Ok))
import Base.SourcePosition (SourcePosition)
import Parsec.Error (ParseError)
import Parsec.Parser (Parser (run))
import Stream.IndexedStream (IndexedStream, currPos)

testRun :: Parser (IndexedStream v) v ok -> IndexedStream v -> (Maybe (SourcePosition v), Result (ParseError v) ok)
testRun parser input =
  run parser input |> \case
    Ok (rest, a) -> (currPos rest, Ok a)
    Error (rest, e) -> (currPos rest, Error e)

pipe :: a -> (a -> b) -> b
pipe x f = f x

(|>) :: a -> (a -> b) -> b
(|>) = pipe