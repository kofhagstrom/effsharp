{-# LANGUAGE FlexibleInstances #-}

module LexerSpec (spec) where

import qualified Base.Result as Result
import Data.Functor.Identity (Identity)
import Lexer (Keyword (..), Literal (..), Token (..), tokens)
import Parsec.Error (ParseError (UnexpectedError))
import Parsec.Parser (Parser)
import qualified Stream.IndexedStream as IndexedStream
import Stream.SourcePosition (SourcePosition)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, Testable (property), listOf, listOf1, oneof, withMaxSuccess)
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Property (forAll)
import TestHelper (testRun)
import Prelude hiding (error)

indexedRun :: Parser (IndexedStream.IndexedStream Char) Char Identity ok -> String -> (Maybe (SourcePosition Char), Result.Result (ParseError Char) ok)
indexedRun parser input = testRun parser (IndexedStream.fromString input)

arbitraryInput :: Gen String
arbitraryInput = do
  let keywords = pad " " <$> ["let", "in", "type", "if", "then", "else"]
  let operators = ["+", "-", "*", "/", "!", "~", "=", "<", ">", ":", ",", "(", ")", "[", "]", "&&", "||", "==", "<=", ">=", "!="]
  let paddedOperators = pad " " <$> operators
  let nonKeywords = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ [' ']
  let stringLiterals = pad "\"" <$> listOf1 (elements nonKeywords)
  ws <- listOf1 $ oneof [stringLiterals, elements keywords, elements operators, elements paddedOperators, listOf1 $ elements nonKeywords]
  return $ unwords ws

pad :: String -> String -> String
pad that this = that ++ this ++ reverse that

prop_tokens :: String -> Bool
prop_tokens input =
  case indexedRun tokens input of
    (_, Result.Ok parsed) -> validateTokens input parsed
    (_, Result.Error _) -> False

validateTokens :: String -> [Token] -> Bool
validateTokens input ts
  | "let" `elem` words input = KeywordT LetKW `elem` ts
  | "in" `elem` words input = KeywordT InKW `elem` ts
  | "type" `elem` words input = KeywordT TypeKW `elem` ts
  | "if" `elem` words input = KeywordT IfKW `elem` ts
  | "then" `elem` words input = KeywordT ThenKW `elem` ts
  | "else" `elem` words input = KeywordT ElseKW `elem` ts
  | "+" `elem` words input = PlusT `elem` ts
  | "-" `elem` words input = MinusT `elem` ts
  | "*" `elem` words input = AsteriskT `elem` ts
  | "/" `elem` words input = DivisionT `elem` ts
  | "!" `elem` words input = BangT `elem` ts
  | "~" `elem` words input = TildeT `elem` ts
  | "=" `elem` words input = LogicalEqualityT `elem` ts
  | "<" `elem` words input = LessThanT `elem` ts
  | ">" `elem` words input = GreaterThanT `elem` ts
  | "<=" `elem` words input = LessThanOrEqualT `elem` ts
  | ">=" `elem` words input = GreaterThanOrEqualT `elem` ts
  | "<>" `elem` words input = NotEqualT `elem` ts
  | "&&" `elem` words input = AndT `elem` ts
  | "||" `elem` words input = OrT `elem` ts
  | ":" `elem` words input = ColonT `elem` ts
  | "," `elem` words input = CommaT `elem` ts
  | "(" `elem` words input = OpenParenthesisT `elem` ts
  | ")" `elem` words input = CloseParenthesisT `elem` ts
  | "[" `elem` words input = OpenBracketT `elem` ts
  | "]" `elem` words input = CloseBracketT `elem` ts
  | input == " " = null ts
  | input == "\n" = null ts
  | otherwise = not (null ts)

spec :: Spec
spec = do
  describe "regressions" $ do
    it "should not parse space followed by digit followed by letter" $ do
      let input = " ==  2jHAFCMyq53hUw77MqUYELtD  m6y1l7DAJwKmASoBuxjJ 4QvxMMfn5Z  !=  != :"
      let expected =
            ( Nothing,
              Result.Error $ UnexpectedError ""
            )
      indexedRun tokens input `shouldBe` expected
  describe "pbt" $ do
    it "tokens" $ property $ withMaxSuccess 1000 $ forAll arbitraryInput prop_tokens
  describe "handPicked" $ do
    it "parses single integer literal" $ do
      let input = "123"
      let expected = (Nothing, Result.Ok [LiteralT (IntL 123)])
      indexedRun tokens input `shouldBe` expected

    it "parses single string literal" $ do
      let input = "\"hello\""
      let expected = (Nothing, Result.Ok [LiteralT (StringL "hello")])
      indexedRun tokens input `shouldBe` expected

    it "parses single identifier" $ do
      let input = "variable"
      let expected = (Nothing, Result.Ok [LiteralT (IdentifierL "variable")])
      indexedRun tokens input `shouldBe` expected

    it "parses multiple tokens" $ do
      let input = "123 + 456"
      let expected = (Nothing, Result.Ok [LiteralT (IntL 123), PlusT, LiteralT (IntL 456)])
      indexedRun tokens input `shouldBe` expected

    it "parses keywords" $ do
      let input = "if else"
      let expected = (Nothing, Result.Ok [KeywordT IfKW, KeywordT ElseKW])
      indexedRun tokens input `shouldBe` expected

    it "parses operators" $ do
      let input = "&& ||"
      let expected = (Nothing, Result.Ok [AndT, OrT])
      indexedRun tokens input `shouldBe` expected

    it "parses mixed tokens" $ do
      let input = "if (x <= 10) then x + 1"
      let expected =
            ( Nothing,
              Result.Ok
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