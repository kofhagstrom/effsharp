{-# LANGUAGE FlexibleContexts #-}

module LexerSpec (spec) where

import IndexedStream (indexedStreamFromString)
import Lexer (digit, digits)
import Parsec (ParseError (..))
import Parser (Parser (run))
import Result (Result (Ok))
import ResultHelper (unwrapError)
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (error)

ok :: Parser a error b -> a -> Result (a, error) b
ok f input = snd <$> run f input

error :: (Show ok) => Result (a, b) ok -> b
error runner = snd $ unwrapError runner

spec :: Spec
spec = do
  describe "misc" $ do
    it "digit_ok" $
      let input = indexedStreamFromString "123hej"
       in ok digit input `shouldBe` Ok '1'
    it "digit_error" $
      let input = indexedStreamFromString "hej"
       in error (run digit input) `shouldBe` [UnexpectedToken 'h']
    it "digits_ok" $
      let input = indexedStreamFromString "123hej"
       in ok digits input `shouldBe` Ok "123"
    it "digits_error" $
      let input = indexedStreamFromString "hej"
       in error (run digits input) `shouldBe` [UnexpectedToken 'h']