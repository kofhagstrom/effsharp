{-# LANGUAGE FlexibleContexts #-}

module Lexer
  ( tokens,
    Token (..),
    Keyword (..),
    Literal (..),
  )
where

import qualified Base.Result as Result
import Control.Applicative (Alternative (many, (<|>)), some)
import Parsec.Error (ParseError (..))
import Parsec.Parsec (Parser, exact, ignore, noneOf, oneOf, or, skip, while)
import qualified Parsec.Parser as Parser
import Stream.Stream (Stream)
import Prelude hiding (or)

type Lexer stream = Parser stream Char Token

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
  | LiteralT Literal
  deriving (Show, Eq)

data Keyword
  = IntKW
  | StringKW
  | IfKW
  | ThenKW
  | ElseKW
  deriving (Show, Eq)

data Literal
  = IntL Integer
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

nonLiteral :: (Stream stream Char) => Lexer stream
nonLiteral = nonLiteral' stringToToken
  where
    nonLiteral' s2t =
      Parser.choice
        [ case s2t of
            [] -> Parser.fail $ UnexpectedError "Could not parse non literal from token"
            (str, t) : rest ->
              Parser.choice [lit str t, nonLiteral' rest]
        ]

    lit str t = do
      skip str
      return t

    stringToToken =
      [ ("int", KeywordT IntKW),
        ("string", KeywordT StringKW),
        ("if", KeywordT IfKW),
        ("else", KeywordT ElseKW),
        ("then", KeywordT ThenKW),
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

literal :: (Stream stream Char, Show stream) => Lexer stream
literal =
  Parser.mapError
    (const . UnexpectedError . ("Could not parse literal from " ++) . show)
    $ Parser.choice [intL, stringL, identifierL]

stringL :: (Stream stream Char, Show stream) => Lexer stream
stringL =
  do
    skip "\""
    s <- many $ noneOf "\""
    skip "\""
    return $ LiteralT $ StringL s

identifierL :: (Stream stream Char, Show stream) => Lexer stream
identifierL =
  do
    first <- letter
    rest <- many (letter `or` digit)
    return $ LiteralT $ IdentifierL (first : rest)

intL :: (Stream stream Char, Show stream) => Lexer stream
intL = do
  ds <- some digit
  result <- Parser.liftResult (Result.read (UnexpectedError $ "Could not parse integer from" ++ ds) ds)
  return $ LiteralT $ IntL result

digit :: (Stream stream Char) => Parser stream Char Char
digit = oneOf ['0' .. '9']

letter :: (Stream stream Char) => Parser stream Char Char
letter = oneOf ['a' .. 'z'] <|> oneOf ['A' .. 'Z']

spaces :: (Stream stream Char) => Parser stream Char String
spaces = while (== ' ')

newLine :: (Stream stream Char) => Parser stream Char String
newLine = exact "\n"

singleLineComment :: (Stream stream Char) => Parser stream Char String
singleLineComment = do
  _ <- exact "//"
  while (/= '\n')

token :: (Stream stream Char, Show stream) => Parser stream Char Token
token =
  Parser.choice
    [ do
        ignore (Parser.choice [newLine, singleLineComment])
        token,
      do
        ignore spaces
        output <- nonLiteral
        ignore spaces
        return output,
      do
        ignore spaces
        output <- literal
        ignore spaces
        return output
    ]

tokens :: (Stream stream Char, Show stream) => Parser stream Char [Token]
tokens = many token