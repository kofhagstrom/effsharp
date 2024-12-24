{-# LANGUAGE FlexibleContexts #-}

module ParserSpec (spec) where

import Expects (error, ok)
import IndexedStream (indexedStreamFromString)
import Parsec (ParseError (..), exact, skip)
import Parser (Parser (run))
import Result (Result (Ok))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (error)

spec :: Spec
spec = do
  describe "misc" $ do
    it "exact_ok" $
      let input = indexedStreamFromString "hej"
       in ok (exact "hej") input `shouldBe` Ok "hej"
    it "skip_ok" $
      let input = indexedStreamFromString "hejhej"
       in ok (skip "hej") input `shouldBe` Ok ()
    it "skip_error" $
      let input = indexedStreamFromString "hejhej"
       in error (run (skip "oj") input) `shouldBe` [UnexpectedToken 'h']