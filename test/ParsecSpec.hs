module ParsecSpec (spec) where

import Base.Result (Result (Error, Ok))
import Base.SourcePosition (mkPos)
import Parsec.Parsec (ParseError (..), Parser, equal, exact, loop, next, skip, someOf, (*>>=))
import Stream.IndexedStream (IndexedStream)
import qualified Stream.IndexedStream as IndexedStream
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun)
import Text.Read (readMaybe)
import Prelude hiding (error, read)

data Token = Number Integer | Comma | FloatingPoint Float deriving (Show, Eq)

digits :: Parser (IndexedStream Char) Char [Char]
digits = someOf "0123456789"

read :: (Read ok) => String -> Result (ParseError t) ok
read token =
  case readMaybe token of
    Just int -> Ok int
    Nothing -> Error $ UnexpectedError $ "Could not read " ++ token

floatingPoint :: Parser (IndexedStream Char) Char Token
floatingPoint = FloatingPoint <$> (floatString *>>= read)
  where
    floatString =
      do
        n <- digits
        _ <- comma
        f <- digits
        return $ n ++ "." ++ f

number :: Parser (IndexedStream Char) Char Token
number = Number <$> (digits *>>= read)

comma :: Parser (IndexedStream Char) Char Token
comma = Comma <$ equal ','

digitAndComma :: Parser (IndexedStream Char) Char [Token]
digitAndComma = do
  n <- number
  c <- comma
  return [n, c]

spec :: Spec
spec = do
  describe "misc" $ do
    it "exact_ok" $
      let input = IndexedStream.fromString "hejhej"
       in testRun (exact "hej") input `shouldBe` (Just (mkPos 1 4 'h'), Ok "hej")
    it "exact_error" $
      let input = IndexedStream.fromString "hej"
       in testRun (exact "hej!") input `shouldBe` (Nothing, Error MissingInput)
    it "loop_ok" $
      let input = IndexedStream.fromString "hejhej"
       in testRun (loop (exact "hej")) input `shouldBe` (Nothing, Ok ["hej", "hej"])
    it "loop_error" $
      let input = IndexedStream.fromString "hej"
       in testRun (loop (equal 'x')) input `shouldBe` (Just (mkPos 1 1 'h'), Error $ UnexpectedToken 'h')
    it "next_ok" $
      let input = IndexedStream.fromString "hej"
       in testRun next input `shouldBe` (Just (mkPos 1 2 'e'), Ok 'h')
    it "next_error" $
      let input = IndexedStream.fromString ""
       in testRun next input `shouldBe` (Nothing, Error MissingInput)
    it "skip_ok" $
      let input = IndexedStream.fromString "hejhej"
       in let skipTwice =
                ( do
                    skip "hej"
                    skip "hej"
                )
           in testRun skipTwice input `shouldBe` (Nothing, Ok ())
    it "skip_error" $
      let input = IndexedStream.fromString "hej"
       in testRun (skip "hej!") input `shouldBe` (Nothing, Error MissingInput)
    it "number_ok" $
      let input = IndexedStream.fromString "12345"
       in testRun number input `shouldBe` (Nothing, Ok $ Number 12345)
    it "number_error" $
      let input = IndexedStream.fromString "hej"
       in testRun number input `shouldBe` (Just (mkPos 1 1 'h'), Error $ UnexpectedToken 'h')
    it "comma_ok" $
      testRun comma (IndexedStream.fromString ",") `shouldBe` (Nothing, Ok Comma)
    it "comma_error" $
      let input = IndexedStream.fromString "hej"
       in testRun comma input `shouldBe` (Just (mkPos 1 1 'h'), Error $ UnexpectedToken 'h')
    it "digitAndComma_ok" $
      let input = IndexedStream.fromString "12345,"
       in testRun digitAndComma input `shouldBe` (Nothing, Ok [Number 12345, Comma])
    it "digitAndComma_error" $
      let input = IndexedStream.fromString "12345"
       in testRun digitAndComma input `shouldBe` (Nothing, Error MissingInput)
    it "floatingPoint_ok" $ do
      let input = IndexedStream.fromString "12345,12345"
      testRun floatingPoint input `shouldBe` (Nothing, Ok $ FloatingPoint 12345.12345)