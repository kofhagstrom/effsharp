{-# LANGUAGE FlexibleContexts #-}

module ParserSpec (spec) where

import Parsec
import Parser
import Result
import Stream
import Test.Hspec

matcher :: Parser (IndexedStream Char) [ParseError] Char
matcher = parseMatch f
  where
    f 'h' = True
    f _ = False

indexedInput :: IndexedStream Char
indexedInput = indexedStreamFromString "hej"

runner :: Char
runner =
  case run matcher indexedInput of
    Error (_, e) -> (error . show) e
    Ok (_, res) -> res

spec :: Spec
spec = do
  describe "misc" $ do
    it "parseMatch" $
      runner `shouldBe` 'h'
