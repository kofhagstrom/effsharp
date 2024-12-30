module LexerSpec (spec) where

import Base.Result (Result (Error, Ok))
import Base.SourcePosition (mkPos)
import Lexer (Literal (..), Token (LiteralT), literal)
import Parsec.Error (ParseError (..))
import qualified Stream.IndexedStream as IndexedStream
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun, (|>))
import Prelude hiding (error)

spec :: Spec
spec = do
  describe "literal" $ do
    it "parses integer literal" $
      let actual = IndexedStream.fromString "123" |> testRun literal
          expected = (Nothing, Ok . LiteralT $ IntL 123)
       in actual `shouldBe` expected
    it "parses string literal" $
      let actual = IndexedStream.fromString "\"hello123\"" |> testRun literal
          expected = (Nothing, Ok . LiteralT $ StringL "hello123")
       in actual `shouldBe` expected
    it "parses identifier literal" $
      let actual = IndexedStream.fromString "abc123" |> testRun literal
          expected = (Nothing, Ok . LiteralT $ IdentifierL "abc123")
       in actual `shouldBe` expected
    it "fails to parse invalid integer literal" $
      let actual = IndexedStream.fromString "123abc" |> testRun literal
          expected = (Nothing, Error (UnexpectedError "Could not parse literal"))
       in actual `shouldBe` expected
    it "fails to parse invalid string literal" $
      let actual = IndexedStream.fromString "\"hello123" |> testRun literal
          expected = (Just $ mkPos 1 1 '\"', Error $ UnexpectedError "Could not parse literal")
       in actual `shouldBe` expected
    it "fails to parse invalid identifier literal" $
      let actual = IndexedStream.fromString "123abc " |> testRun literal
          expected = (Nothing, Error (UnexpectedError "Could not parse literal from \"123abc \""))
       in actual `shouldBe` expected
