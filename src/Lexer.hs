module Lexer (Lexer) where

import Parsec (ParseError)
import Parser (Parser)
import Stream (IndexedStream)

type Lexer a = Parser (IndexedStream Char) [ParseError] a
