{-# LANGUAGE FlexibleContexts #-}

module Lexer (Lexer, digit, digits, letter, letters, letterOrDigit, lettersOrDigits, number) where

import Base.Result (Result (Error, Ok))
import Control.Applicative (some)
import Parsec.Error (ParseError (..))
import Parsec.Parsec (Parser, manyOf, oneOf, or, (*>>=))
import Stream.IndexedStream (IndexedStream)
import Stream.Stream
import Text.Read (readMaybe)
import Prelude hiding (or)

type Lexer a = Parser (IndexedStream Char) a

digitChars :: String
digitChars = "1234567890"

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

digit :: (Stream stream Char) => Parser stream Char Char
digit = oneOf digitChars

digits :: (Stream stream Char, Monoid stream) => Parser stream Char String
digits = manyOf digitChars

letter :: (Stream stream Char) => Parser stream Char Char
letter = oneOf alphabet

letters :: (Stream stream Char, Monoid stream) => Parser stream Char String
letters = manyOf alphabet

letterOrDigit :: (Stream stream Char, Monoid stream) => Parser stream Char Char
letterOrDigit = letter `or` digit

lettersOrDigits :: (Stream stream Char, Monoid stream) => Parser stream Char String
lettersOrDigits = some letterOrDigit

number :: Parser (IndexedStream Char) Char Integer
number = digits *>>= readInt
  where
    readInt token =
      case readMaybe token of
        Just int -> Ok int
        Nothing -> Error $ UnexpectedToken (head token)
