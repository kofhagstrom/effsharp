module ParsecSpec (spec) where

import Base.Result (Result (Error, Ok))
import Parsec.Parsec (ParseError (..), Parser, equal, exact, loop, next, skip, someOf, (*>>=))
import Stream.IndexedStream (IndexedStream, indexedStreamFromString)
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
      let input = indexedStreamFromString "hej"
       in testRun (exact "hej") input `shouldBe` Ok "hej"
    it "exact_error" $
      let input = indexedStreamFromString "hhej"
       in testRun (exact "hej") input `shouldBe` Error (UnexpectedToken 'h')
    it "skip_ok" $
      let input = indexedStreamFromString "hejhej"
       in testRun (skip "hej") input `shouldBe` Ok ()
    it "skip_error" $
      let input = indexedStreamFromString "hejhej"
       in testRun (skip "oj") input `shouldBe` Error (UnexpectedToken 'h')
    it "loop_ok" $
      let input = indexedStreamFromString "1,2,3"
       in testRun (concat <$> loop digitAndComma) input `shouldBe` Ok [Number 1, Comma, Number 2, Comma]
    it "next_ok" $
      let input = indexedStreamFromString "hej"
       in testRun next input `shouldBe` Ok 'h'
    it "next_error" $
      let input = indexedStreamFromString ""
       in testRun next input `shouldBe` Error MissingInput
