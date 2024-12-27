module LexerSpec (spec) where

import Base.Result (Result (Error, Ok))
import Lexer (digit, number)
import Parsec.Error (ParseError (..))
import Stream.IndexedStream (indexedStreamFromString, mkPos)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun, (|>))
import Prelude hiding (error)

spec :: Spec
spec = do
  describe "misc" $ do
    it "digit_ok" $
      let actual = indexedStreamFromString "123hej" |> testRun digit
          expected = (Just $ mkPos 1 2 '2', Ok '1')
       in actual `shouldBe` expected
    it "digit_error" $
      let actual = indexedStreamFromString "hej" |> testRun digit
          expected = (Just $ mkPos 1 1 'h', Error (UnexpectedToken 'h'))
       in actual `shouldBe` expected
    it "number_ok" $
      let actual = indexedStreamFromString "12345" |> testRun number
          expected = (Nothing, Ok 12345)
       in actual `shouldBe` expected
