module LexerSpec (spec) where

import Base.Result (Result (Ok))
import Base.SourcePosition (SourcePosition)
import Lexer (Keyword (..), Literal (..), Token (..), tokens)
import Parsec.Error (ParseError)
import Parsec.Parsec (Parser)
import qualified Stream.IndexedStream as IndexedStream
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun)
import Prelude hiding (error)

indexedRun :: Parser (IndexedStream.IndexedStream Char) Char ok -> String -> (Maybe (SourcePosition Char), Result (ParseError Char) ok)
indexedRun parser input = testRun parser (IndexedStream.fromString input)

spec :: Spec
spec = do
  describe "tokens" $ do
    it "parses single integer literal" $ do
      let input = "123"
      let expected = (Nothing, Ok [LiteralT (IntL 123)])
      indexedRun tokens input `shouldBe` expected

    it "parses single string literal" $ do
      let input = "\"hello\""
      let expected = (Nothing, Ok [LiteralT (StringL "hello")])
      indexedRun tokens input `shouldBe` expected

    it "parses single identifier" $ do
      let input = "variable"
      let expected = (Nothing, Ok [LiteralT (IdentifierL "variable")])
      indexedRun tokens input `shouldBe` expected

    it "parses multiple tokens" $ do
      let input = "123 + 456"
      let expected = (Nothing, Ok [LiteralT (IntL 123), PlusT, LiteralT (IntL 456)])
      indexedRun tokens input `shouldBe` expected

    it "parses keywords" $ do
      let input = "if else"
      let expected = (Nothing, Ok [KeywordT IfKW, KeywordT ElseKW])
      indexedRun tokens input `shouldBe` expected

    it "parses operators" $ do
      let input = "&& ||"
      let expected = (Nothing, Ok [AndT, OrT])
      indexedRun tokens input `shouldBe` expected

    it "parses mixed tokens" $ do
      let input = "if (x <= 10) then x + 1"
      let expected =
            ( Nothing,
              Ok
                [ KeywordT IfKW,
                  OpenParenthesisT,
                  LiteralT (IdentifierL "x"),
                  LessThanOrEqualT,
                  LiteralT (IntL 10),
                  CloseParenthesisT,
                  KeywordT ThenKW,
                  LiteralT (IdentifierL "x"),
                  PlusT,
                  LiteralT (IntL 1)
                ]
            )
      indexedRun tokens input `shouldBe` expected
