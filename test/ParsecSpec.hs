module ParsecSpec (spec) where

import Base.Result (Result (Error, Ok))
import qualified Base.Result as Result
import Data.Functor.Identity (Identity)
import Parsec.Parsec (ParseError (..), Parser, equal, exact, loop, next, skip, someOf, (*>>=))
import Stream.IndexedStream (IndexedStream)
import qualified Stream.IndexedStream as IndexedStream
import Stream.SourcePosition (mkPos)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun)
import Prelude hiding (error, read)

data Token = Number Integer | Comma | FloatingPoint Float deriving (Show, Eq)

type TestParser a = Parser (IndexedStream Char) Char Identity a

floatingPoint :: TestParser Token
floatingPoint = FloatingPoint <$> (floatString *>>= (\token -> Result.read (UnexpectedError $ "Could not read" ++ show token) token))
  where
    floatString =
      do
        n <- someOf ['0' .. '9']
        _ <- comma
        f <- someOf ['0' .. '9']
        return $ n ++ "." ++ f

number :: TestParser Token
number = Number <$> (someOf ['0' .. '9'] *>>= (\token -> Result.read (UnexpectedError $ "Could not read" ++ show token) token))

comma :: TestParser Token
comma = Comma <$ equal ','

digitAndComma :: TestParser [Token]
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