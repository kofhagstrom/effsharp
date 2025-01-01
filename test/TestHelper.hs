{-# LANGUAGE LambdaCase #-}

module TestHelper (testRun, (|>)) where

import Base.Result (Result (Error, Ok))
import Data.Functor.Identity (Identity (Identity))
import Parsec.Error (ParseError)
import Parsec.Parser (Parser (run))
import Stream.IndexedStream (IndexedStream, currPos)
import Stream.SourcePosition (SourcePosition)

testRun :: Parser (IndexedStream v) v Identity ok -> IndexedStream v -> (Maybe (SourcePosition v), Result (ParseError v) ok)
testRun parser input =
  run parser input |> \case
    Identity (Ok (rest, a)) -> (currPos rest, Ok a)
    Identity (Error (rest, e)) -> (currPos rest, Error e)

pipe :: a -> (a -> b) -> b
pipe x f = f x

(|>) :: a -> (a -> b) -> b
(|>) = pipe