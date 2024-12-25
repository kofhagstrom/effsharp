{-# LANGUAGE FlexibleContexts #-}

module LexerSpec (spec) where

import Base.Result (Result (Error, Ok))
import Lexer (digit, digits, letterOrDigit, lettersOrDigits, number)
import Parsec.Error (ParseError (..))
import Stream.IndexedStream (indexedStreamFromString)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun)
import Prelude hiding (error)

spec :: Spec
spec = do
  describe "misc" $ do
    it "digit_ok" $
      let input = indexedStreamFromString "123hej"
       in testRun digit input `shouldBe` Ok '1'
    it "digit_error" $
      let input = indexedStreamFromString "hej"
       in testRun digit input `shouldBe` Error [UnexpectedToken 'h']
    it "digits_ok" $
      let input = indexedStreamFromString "123hej"
       in testRun digits input `shouldBe` Ok "123"
    it "digits_error" $
      let input = indexedStreamFromString "hej"
       in testRun digits input `shouldBe` Error [UnexpectedToken 'h']
    it "lettersOrDigits_ok" $
      let input = indexedStreamFromString "1h_"
       in testRun lettersOrDigits input `shouldBe` Ok "1h"
    it "lettersOrDigits_error" $
      let input = indexedStreamFromString "_1h"
       in testRun letterOrDigit input `shouldBe` Error [UnexpectedToken '_']
    it "number_ok" $
      let input = indexedStreamFromString "12345"
       in testRun number input `shouldBe` Ok 12345