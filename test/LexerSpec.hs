module LexerSpec (spec) where

import Base.Result (Result (Error, Ok))
import Lexer (Keyword (..), Literal (..), Token (..), digit, digits, letterOrDigit, lettersOrDigits, literal, nonLiteral, number)
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
      indexedStreamFromString "hej" |> testRun digit `shouldBe` Error (UnexpectedToken 'h')
    it "digits_ok" $
      indexedStreamFromString "123hej" |> testRun digits `shouldBe` Ok "123"
    it "digits_error" $
      indexedStreamFromString "hej" |> testRun digits `shouldBe` Error (UnexpectedToken 'h')
    it "lettersOrDigits_ok" $
      indexedStreamFromString "1h_" |> testRun lettersOrDigits `shouldBe` Ok "1h"
    it "lettersOrDigits_error" $
      indexedStreamFromString "_1h" |> testRun letterOrDigit `shouldBe` Error (MultipleError [UnexpectedToken '_', UnexpectedToken '_'])
    it "number_ok" $
      indexedStreamFromString "12345" |> testRun number `shouldBe` Ok 12345
    it "nonLiteral_int" $
      indexedStreamFromString "int" |> testRun nonLiteral `shouldBe` Ok (KeywordT IntKW)
    it "nonLiteral_string" $
      indexedStreamFromString "string" |> testRun nonLiteral `shouldBe` Ok (KeywordT StringKW)
    it "nonLiteral_if" $
      indexedStreamFromString "if" |> testRun nonLiteral `shouldBe` Ok (KeywordT IfKW)
    it "nonLiteral_else" $
      indexedStreamFromString "else" |> testRun nonLiteral `shouldBe` Ok (KeywordT ElseKW)
    it "nonLiteral_return" $
      indexedStreamFromString "return" |> testRun nonLiteral `shouldBe` Ok (KeywordT ReturnKW)
    it "nonLiteral_for" $
      indexedStreamFromString "for" |> testRun nonLiteral `shouldBe` Ok (KeywordT ForKW)
    it "nonLiteral_while" $
      indexedStreamFromString "while" |> testRun nonLiteral `shouldBe` Ok (KeywordT WhileKW)
    it "nonLiteral_do" $
      indexedStreamFromString "do" |> testRun nonLiteral `shouldBe` Ok (KeywordT DoKW)
    it "nonLiteral_break" $
      indexedStreamFromString "break" |> testRun nonLiteral `shouldBe` Ok (KeywordT BreakKW)
    it "nonLiteral_continue" $
      indexedStreamFromString "continue" |> testRun nonLiteral `shouldBe` Ok (KeywordT ContinueKW)
    it "nonLiteral_equals" $
      indexedStreamFromString "=" |> testRun nonLiteral `shouldBe` Ok LogicalEqualityT
    it "nonLiteral_notEqual" $
      indexedStreamFromString "<>" |> testRun nonLiteral `shouldBe` Ok NotEqualT
    it "nonLiteral_bang" $
      indexedStreamFromString "!" |> testRun nonLiteral `shouldBe` Ok BangT
    it "nonLiteral_lessThanOrEqual" $
      indexedStreamFromString "<=" |> testRun nonLiteral `shouldBe` Ok LessThanOrEqualT
    it "nonLiteral_lessThan" $
      indexedStreamFromString "<" |> testRun nonLiteral `shouldBe` Ok LessThanT
    it "nonLiteral_greaterThanOrEqual" $
      indexedStreamFromString ">=" |> testRun nonLiteral `shouldBe` Ok GreaterThanOrEqualT
    it "nonLiteral_greaterThan" $
      indexedStreamFromString ">" |> testRun nonLiteral `shouldBe` Ok GreaterThanT
    it "nonLiteral_and" $
      indexedStreamFromString "&&" |> testRun nonLiteral `shouldBe` Ok AndT
    it "nonLiteral_or" $
      indexedStreamFromString "||" |> testRun nonLiteral `shouldBe` Ok OrT
    it "nonLiteral_openParenthesis" $
      indexedStreamFromString "(" |> testRun nonLiteral `shouldBe` Ok OpenParenthesisT
    it "nonLiteral_closeParenthesis" $
      indexedStreamFromString ")" |> testRun nonLiteral `shouldBe` Ok CloseParenthesisT
    it "nonLiteral_minus" $
      indexedStreamFromString "-" |> testRun nonLiteral `shouldBe` Ok MinusT
    it "nonLiteral_tilde" $
      indexedStreamFromString "~" |> testRun nonLiteral `shouldBe` Ok TildeT
    it "nonLiteral_plus" $
      indexedStreamFromString "+" |> testRun nonLiteral `shouldBe` Ok PlusT
    it "nonLiteral_asterisk" $
      indexedStreamFromString "*" |> testRun nonLiteral `shouldBe` Ok AsteriskT
    it "nonLiteral_division" $
      indexedStreamFromString "/" |> testRun nonLiteral `shouldBe` Ok DivisionT
    it "nonLiteral_questionMark" $
      indexedStreamFromString "?" |> testRun nonLiteral `shouldBe` Ok QuestionMarkT
    it "nonLiteral_colon" $
      indexedStreamFromString ":" |> testRun nonLiteral `shouldBe` Ok ColonT
    it "nonLiteral_comma" $
      indexedStreamFromString "," |> testRun nonLiteral `shouldBe` Ok CommaT
    it "literal_int" $
      indexedStreamFromString "12345" |> testRun literal `shouldBe` Ok (IntL 12345)
    it "literal_string" $
      indexedStreamFromString "\"hello123\"" |> testRun literal `shouldBe` Ok (StringL "hello123")
    it "literal_identifier" $
      indexedStreamFromString "hello123" |> testRun literal `shouldBe` Ok (IdentifierL "hello123")
