module ParsecSpec (spec) where

import Base.Result (Result (Error, Ok))
import Parsec.Parsec (ParseError (..), Parser, equal, exact, loop, next, skip, someOf, (*>>=))
import Stream.IndexedStream (IndexedStream (IndexedStream), indexedStreamFromString, mkPos)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun)
import Text.Read (readMaybe)
import Prelude hiding (error, read)

data Token = Number Integer | Comma deriving (Show, Eq)

number :: Parser (IndexedStream Char) Char Token
number = Number <$> (someOf "0123456789" *>>= readInt)
  where
    readInt token =
      case readMaybe token of
        Just int -> Ok int
        Nothing -> Error $ UnexpectedError $ "Could not parse integer from " ++ token

comma :: Parser (IndexedStream Char) Char Token
comma = Comma <$ equal ','

digitAndComma :: Parser (IndexedStream Char) Char [Token]
digitAndComma =
  do
    n <- number
    c <- comma
    return [n, c]

spec :: Spec
spec = do
  describe "misc" $ do
    it "exact_ok" $
      let input = indexedStreamFromString "hejhej"
       in testRun (exact "hej") input `shouldBe` (Just (mkPos 1 4 'h'), Ok "hej")
    it "exact_error" $
      let input = indexedStreamFromString "hej"
       in testRun (exact "hej!") input `shouldBe` (Nothing, Error MissingInput)
    it "loop_ok" $
      let input = indexedStreamFromString "hejhej"
       in testRun (loop (exact "hej")) input `shouldBe` (Nothing, Ok ["hej", "hej"])
    it "loop_error" $
      let input = indexedStreamFromString "hej"
       in testRun (loop (equal 'x')) input `shouldBe` (Just (mkPos 1 1 'h'), Error $ UnexpectedToken 'h')
    it "next_ok" $
      let input = indexedStreamFromString "hej"
       in testRun next input `shouldBe` (Just (mkPos 1 2 'e'), Ok 'h')
    it "next_error" $
      let input = indexedStreamFromString ""
       in testRun next input `shouldBe` (Nothing, Error MissingInput)
    it "skip_ok" $
      let input = indexedStreamFromString "hejhej"
       in let skipTwice = skip "hej" >> skip "hej"
           in testRun skipTwice input `shouldBe` (Nothing, Ok ())
    it "skip_error" $
      let input = indexedStreamFromString "hej"
       in testRun (skip "hej!") input `shouldBe` (Nothing, Error MissingInput)
    it "number_ok" $
      let input = indexedStreamFromString "12345"
       in testRun number input `shouldBe` (Nothing, Ok $ Number 12345)
    it "number_error" $
      let input = indexedStreamFromString "hej"
       in testRun number input `shouldBe` (Just (mkPos 1 1 'h'), Error $ UnexpectedToken 'h')
    it "comma_ok" $ testRun comma (indexedStreamFromString ",") `shouldBe` (Nothing, Ok Comma)
    it "comma_error" $
      let input = indexedStreamFromString "hej"
       in testRun comma input `shouldBe` (Just (mkPos 1 1 'h'), Error $ UnexpectedToken 'h')
    it "digitAndComma_ok" $
      let input = indexedStreamFromString "12345,"
       in testRun digitAndComma input `shouldBe` (Nothing, Ok [Number 12345, Comma])
    it "digitAndComma_error" $
      let input = indexedStreamFromString "12345"
       in testRun digitAndComma input `shouldBe` (Nothing, Error MissingInput)
    it "indexedStreamFromString" $
      let input = indexedStreamFromString "hej\nhej"
       in input `shouldBe` IndexedStream [mkPos 1 1 'h', mkPos 1 2 'e', mkPos 1 3 'j', mkPos 2 1 'h', mkPos 2 2 'e', mkPos 2 3 'j']