{-# LANGUAGE FlexibleContexts #-}

module ParserSpec (spec) where

import Parsec (ParseError, noneOf)
import Parser (Parser (run))
import Result (Result)
import ResultHelper (unwrapOk)
import Stream (IndexedStream, indexedStreamFromString)
import Test.Hspec (Spec, describe, it, shouldBe)

digits :: Parser (IndexedStream Char) [ParseError] Char
digits = noneOf "1234567890"

indexedInput :: IndexedStream Char
indexedInput = indexedStreamFromString "123hhhej"

runThis :: Result (IndexedStream Char, [ParseError]) (IndexedStream Char, Char)
runThis = run digits indexedInput

spec :: Spec
spec = do
  describe "misc" $ do
    it "parseMatch" $
      unwrapOk (snd <$> runThis) `shouldBe` '1'
