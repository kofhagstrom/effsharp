{-# LANGUAGE FlexibleContexts #-}

module Lexer (Lexer, digit, digits, letter, letters, letterOrDigit, lettersOrDigits) where

import Control.Applicative (some)
import IndexedStream (IndexedStream)
import Parsec (ParseError, oneOf, or)
import Parser (Parser)
import Stream
import Prelude hiding (or)

type Lexer a = Parser (IndexedStream Char) [ParseError a] a

digit :: (Stream stream Char) => Parser stream [ParseError Char] Char
digit = oneOf "1234567890"

digits :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
digits = some digit

letter :: (Stream stream Char) => Parser stream [ParseError Char] Char
letter = oneOf "abcdefghijklmnopqrstuvwxyz"

letters :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
letters = some letter

letterOrDigit :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] Char
letterOrDigit = letter `or` digit

lettersOrDigits :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
lettersOrDigits = some letterOrDigit