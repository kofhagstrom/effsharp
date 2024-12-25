{-# LANGUAGE FlexibleContexts #-}

module LexerSpec (spec) where

import Expects (error, ok)
import IndexedStream (indexedStreamFromString)
import Lexer (digit, digits, letterOrDigit, lettersOrDigits, number)
import Parsec (ParseError (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (error)

spec :: Spec
spec = do
  describe "misc" $ do
    it "digit_ok" $
      let input = indexedStreamFromString "123hej"
       in ok digit input `shouldBe` '1'
    it "digit_error" $
      let input = indexedStreamFromString "hej"
       in error digit input `shouldBe` [UnexpectedToken 'h']
    it "digits_ok" $
      let input = indexedStreamFromString "123hej"
       in ok digits input `shouldBe` "123"
    it "digits_error" $
      let input = indexedStreamFromString "hej"
       in error digits input `shouldBe` [UnexpectedToken 'h']
    it "lettersOrDigits_ok" $
      let input = indexedStreamFromString "1h_"
       in ok lettersOrDigits input `shouldBe` "1h"
    it "lettersOrDigits_error" $
      let input = indexedStreamFromString "_1h"
       in error letterOrDigit input `shouldBe` [UnexpectedToken '_']
    it "number_ok" $
      let input = indexedStreamFromString "12345"
       in ok number input `shouldBe` 12345