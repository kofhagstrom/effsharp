{-# LANGUAGE FlexibleContexts #-}

module Lexer (Lexer, digit, digits, letter, letters) where

import Control.Applicative (some)
import IndexedStream (IndexedStream)
import Parsec (ParseError, oneOf)
import Parser (Parser)
import Stream

type Lexer a = Parser (IndexedStream Char) [ParseError a] a

digit :: (Stream stream Char) => Parser stream [ParseError Char] Char
digit = oneOf "1234567890"

digits :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
digits = some digit

letter :: (Stream stream Char) => Parser stream [ParseError Char] Char
letter = oneOf "abcdefghijklmnopqrstuvwxyz"

letters :: (Stream stream Char, Monoid stream) => Parser stream [ParseError Char] String
letters = some letter