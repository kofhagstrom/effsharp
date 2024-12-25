{-# LANGUAGE FlexibleContexts #-}

module Lexer (Lexer, digit, digits, letter, letters, letterOrDigit, lettersOrDigits, number) where

import Control.Applicative (some)
import IndexedStream (IndexedStream)
import Parsec (ParseError (UnexpectedToken), manyOf, oneOf, or)
import Parser (Parser, (*>>=))
import Result (Result (Error, Ok))
import Stream
import Text.Read (readMaybe)
import Prelude hiding (or)

type Lexer a = Parser (IndexedStream Char) [ParseError a] a

digitChars :: String
digitChars = "1234567890"

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

digit :: (Stream stream Char) => Parser stream [ParseError Char] Char
digit = oneOf digitChars

digits :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
digits = manyOf digitChars

letter :: (Stream stream Char) => Parser stream [ParseError Char] Char
letter = oneOf alphabet

letters :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
letters = manyOf alphabet

letterOrDigit :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] Char
letterOrDigit = letter `or` digit

lettersOrDigits :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
lettersOrDigits = some letterOrDigit

number :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] Integer
number = manyOf "0123456789" *>>= readInt
  where
    readInt token =
      case readMaybe token of
        Just int -> Ok int
        Nothing -> Error [UnexpectedToken . head $ token]
