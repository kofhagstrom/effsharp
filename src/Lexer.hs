module Lexer (Lexer) where

import IndexedStream (IndexedStream)
import Parsec (ParseError)
import Parser (Parser)

type Lexer a = Parser (IndexedStream Char) [ParseError a] a
