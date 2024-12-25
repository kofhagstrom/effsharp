{-# LANGUAGE FlexibleContexts #-}

module LexerSpec (spec) where

import Base.Result (Result (Error, Ok))
import Lexer (digit, digits, letterOrDigit, lettersOrDigits, number)
import Parsec.Error (ParseError (..))
import Stream.IndexedStream (indexedStreamFromString)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun, (|>))
import Prelude hiding (error)

spec :: Spec
spec = do
  describe "misc" $ do
    it "digit_ok" $
      indexedStreamFromString "123hej" |> testRun digit `shouldBe` Ok '1'
    it "digit_error" $
      indexedStreamFromString "hej" |> testRun digit `shouldBe` Error [UnexpectedToken]
    it "digits_ok" $
      indexedStreamFromString "123hej" |> testRun digits `shouldBe` Ok "123"
    it "digits_error" $
      indexedStreamFromString "hej" |> testRun digits `shouldBe` Error [UnexpectedToken]
    it "lettersOrDigits_ok" $
      indexedStreamFromString "1h_" |> testRun lettersOrDigits `shouldBe` Ok "1h"
    it "lettersOrDigits_error" $
      indexedStreamFromString "_1h" |> testRun letterOrDigit `shouldBe` Error [UnexpectedToken]
    it "number_ok" $
      indexedStreamFromString "12345" |> testRun number `shouldBe` Ok 12345