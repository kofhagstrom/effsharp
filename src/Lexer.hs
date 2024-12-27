{-# LANGUAGE FlexibleContexts #-}

module Lexer (digit, letter, number, nonLiteral, Token (..), Keyword (..), literal, Literal (..), comment) where

import Base.Result (Result (Error, Ok))
import Control.Applicative (some)
import GHC.Base (Alternative (empty, (<|>)))
import Parsec.Error (ParseError (..))
import Parsec.Parsec (Parser, exact, oneOf, or, while, (*>>=))
import Stream.Stream (Stream)
import Text.Read (readMaybe)
import Prelude hiding (or)

type Lexer stream ok = Parser stream Char ok

data Token
  = OpenParenthesisT
  | CloseParenthesisT
  | PlusT
  | AsteriskT
  | DivisionT
  | MinusT
  | BangT
  | TildeT
  | LogicalEqualityT
  | InequalityT
  | LessThanT
  | GreaterThanT
  | GreaterThanOrEqualT
  | LessThanOrEqualT
  | NotEqualT
  | AndT
  | OrT
  | ColonT
  | CommaT
  | CommentT String
  | KeywordT Keyword
  deriving (Show, Eq)

data Keyword
  = IntKW
  | StringKW
  | IfKW
  | ElseKW
  deriving (Show, Eq)

nonLiteral :: (Stream stream Char) => Lexer stream Token
nonLiteral = nonLiteral' stringToToken
  where
    stringToToken =
      [ ("int", KeywordT IntKW),
        ("string", KeywordT StringKW),
        ("if", KeywordT IfKW),
        ("else", KeywordT ElseKW),
        ("=", LogicalEqualityT),
        ("<>", NotEqualT),
        ("!", BangT),
        ("<=", LessThanOrEqualT),
        ("<", LessThanT),
        (">=", GreaterThanOrEqualT),
        (">", GreaterThanT),
        ("&&", AndT),
        ("||", OrT),
        ("(", OpenParenthesisT),
        (")", CloseParenthesisT),
        ("-", MinusT),
        ("~", TildeT),
        ("+", PlusT),
        ("*", AsteriskT),
        ("/", DivisionT),
        (":", ColonT),
        (",", CommaT)
      ]
    nonLiteral' ((str, t) : rest) =
      ( do
          _ <- exact str
          return t
      )
        <|> nonLiteral' rest
    nonLiteral' [] = empty

data Literal
  = IntL Integer
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

literal :: (Stream stream Char) => Lexer stream Literal
literal = intL <|> stringL <|> identifierL
  where
    letterOrDigit = some (letter `or` digit)
    intL = IntL <$> number
    stringL = StringL <$> (exact "\"" *> letterOrDigit <* exact "\"")
    identifierL =
      IdentifierL
        <$> ( do
                first <- letter
                rest <- letterOrDigit
                return (first : rest)
            )

digit :: (Stream stream Char) => Lexer stream Char
digit = oneOf "1234567890"

letter :: (Stream stream Char) => Lexer stream Char
letter = oneOf "abcdefghijklmnopqrstuvwxyz"

number :: (Stream stream Char) => Lexer stream Integer
number = some digit *>>= readInt
  where
    readInt input =
      case readMaybe input of
        Just int -> Ok int
        Nothing -> Error $ UnexpectedError $ "Could not parse integer from " ++ input

comment :: (Stream stream Char) => Lexer stream String
comment = exact "//" *> while (/= '\n') <* exact "\n"