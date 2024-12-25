{-# LANGUAGE FlexibleContexts #-}

module ParsecSpec (spec) where

import Base.Result (Result (Error, Ok), mapError)
import IndexedStream (IndexedStream, indexedStreamFromString)
import Parsec.Error (ParseError (..))
import Parsec.Parsec (condition, exact, loop, manyOf, skip)
import Parsec.Parser (Parser (run), (*>>=))
import Test.Hspec (Spec, describe, it, shouldBe)
import TestHelper (testRun)
import Text.Read (readMaybe)
import Prelude hiding (error, read)

data Token = Number Integer | Comma deriving (Show, Eq)

number :: Parser (IndexedStream Char) [ParseError Char] Token
number = manyOf "0123456789" *>>= readInt
  where
    readInt token =
      case readMaybe token of
        Just int -> Ok $ Number int
        Nothing -> Error [UnexpectedToken (head token)]

comma :: Parser (IndexedStream Char) [ParseError Char] Token
comma = Comma <$ condition ','

spec :: Spec
spec = do
  describe "misc" $ do
    it "exact_ok" $
      let input = indexedStreamFromString "hej"
       in testRun (exact "hej") input `shouldBe` Ok "hej"
    it "exact_error" $
      let input = indexedStreamFromString "hhej"
       in testRun (exact "hej") input `shouldBe` Error [UnexpectedToken 'h']
    it "skip_ok" $
      let input = indexedStreamFromString "hejhej"
       in testRun (skip "hej") input `shouldBe` Ok ()
    it "skip_error" $
      let input = indexedStreamFromString "hejhej"
       in testRun (skip "oj") input `shouldBe` Error [UnexpectedToken 'h']
    it "loop_ok" $
      let input = indexedStreamFromString "1,2,3"
       in testRun (loop number comma) input `shouldBe` Ok [Number 1, Comma, Number 2, Comma, Number 3]