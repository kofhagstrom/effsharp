{-# LANGUAGE FlexibleContexts #-}

module Lexer (digit, digits, letter, letters, letterOrDigit, lettersOrDigits, number, nonLiteral, Token (..), Keyword (..), literal, Literal (..)) where

import Base.Result (Result (Error, Ok))
import Control.Applicative (some)
import GHC.Base (Alternative (empty, (<|>)))
import Parsec.Error (ParseError (..))
import Parsec.Parsec (Parser, exact, oneOf, or, someOf, (*>>=))
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
  | QuestionMarkT
  | CommaT
  | CommentT String
  | KeywordT Keyword
  deriving (Show, Eq)

data Keyword
  = IntKW
  | StringKW
  | ReturnKW
  | IfKW
  | ElseKW
  | ForKW
  | DoKW
  | WhileKW
  | BreakKW
  | ContinueKW
  deriving (Show, Eq)

nonLiteral :: (Stream stream Char) => Lexer stream Token
nonLiteral = nonLiteral' stringToToken
  where
    stringToToken =
      [ ("int", KeywordT IntKW),
        ("string", KeywordT StringKW),
        ("if", KeywordT IfKW),
        ("else", KeywordT ElseKW),
        ("return", KeywordT ReturnKW),
        ("for", KeywordT ForKW),
        ("while", KeywordT WhileKW),
        ("do", KeywordT DoKW),
        ("break", KeywordT BreakKW),
        ("continue", KeywordT ContinueKW),
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
        ("?", QuestionMarkT),
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
    intL = IntL <$> number
    stringL = StringL <$> (exact "\"" *> lettersOrDigits <* exact "\"")
    identifierL = IdentifierL <$> lettersOrDigits

digitChars :: String
digitChars = "1234567890"

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

digit :: (Stream stream Char) => Lexer stream Char
digit = oneOf digitChars

digits :: (Stream stream Char) => Lexer stream String
digits = someOf digitChars

letter :: (Stream stream Char) => Lexer stream Char
letter = oneOf alphabet

letters :: (Stream stream Char) => Lexer stream String
letters = someOf alphabet

letterOrDigit :: (Stream stream Char) => Lexer stream Char
letterOrDigit = letter `or` digit

lettersOrDigits :: (Stream stream Char) => Lexer stream String
lettersOrDigits = some letterOrDigit

number :: (Stream stream Char) => Lexer stream Integer
number = digits *>>= readInt
  where
    readInt ds =
      case readMaybe ds of
        Just int -> Ok int
        Nothing -> Error $ UnexpectedError $ "Could not parse integer from " ++ ds
