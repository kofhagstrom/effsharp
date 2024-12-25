{-# LANGUAGE FlexibleContexts #-}

module ParsecSpec (spec) where

import Expects (error, ok)
import IndexedStream (IndexedStream, indexedStreamFromString)
import Parsec (ParseError (..), exact, loop, manyOf, match, skip)
import Parser (Parser, (*>>=))
import Result (Result (Error, Ok))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Read (readMaybe)
import Prelude hiding (error, read)

data Token = Digit Integer | Comma deriving (Show, Eq)

digit :: Parser (IndexedStream Char) [ParseError Char] Token
digit = manyOf "0123456789" *>>= readInt
  where
    readInt token =
      case readMaybe token of
        Just int -> Ok $ Digit int
        Nothing -> Error [UnexpectedToken (head token)]

comma :: Parser (IndexedStream Char) [ParseError Char] Token
comma = Comma <$ match ','

spec :: Spec
spec = do
  describe "misc" $ do
    it "exact_ok" $
      let input = indexedStreamFromString "hej"
       in ok (exact "hej") input `shouldBe` "hej"
    it "exact_error" $
      let input = indexedStreamFromString "hhej"
       in error (exact "hej") input `shouldBe` [UnexpectedToken 'h']
    it "skip_ok" $
      let input = indexedStreamFromString "hejhej"
       in ok (skip "hej") input `shouldBe` ()
    it "skip_error" $
      let input = indexedStreamFromString "hejhej"
       in error (skip "oj") input `shouldBe` [UnexpectedToken 'h']
    it "loop_ok" $
      let input = indexedStreamFromString "1,2,3"
       in ok (loop digit comma) input `shouldBe` [Digit 1, Comma, Digit 2, Comma, Digit 3]