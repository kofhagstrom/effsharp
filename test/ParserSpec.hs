{-# LANGUAGE FlexibleContexts #-}

module ParserSpec (spec) where

import Parsec
import Parser
import ResultHelper (unwrapOk)
import Stream
import Test.Hspec

parseH :: Parser (IndexedStream Char) error String
parseH = while isH
  where
    isH 'h' = True
    isH _ = False

indexedInput :: IndexedStream Char
indexedInput = indexedStreamFromString "hhhej"

spec :: Spec
spec = do
  describe "misc" $ do
    it "parseMatch" $
      unwrapOk (snd <$> run parseH indexedInput) `shouldBe` "hhh"
