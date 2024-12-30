{-# LANGUAGE FlexibleContexts #-}

module Lexer (nonLiteral, Token (..), Keyword (..), literal, Literal (..)) where

import qualified Base.Result as Result
import Control.Applicative (some)
import GHC.Base (Alternative (many, (<|>)))
import Parsec.Error (ParseError (..))
import Parsec.Parsec (Parser, exact, ignore, oneOf, or, skip, while)
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
              Parser.choice [lit str t, nonLiteral' rest],
          singleLineComment,
          multiLineComment
        ]
    lit str t = do
      _ <- exact str
      return t
    singleLineComment = do
      _ <- exact "//"
      CommentT <$> while (/= '\n')
    multiLineComment = do
      _ <- exact "/*"
      c <- CommentT <$> while (/= '*')
      _ <- exact "*/"
      return c
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

literal :: (Stream stream Char, Show stream) => Lexer stream
literal =
  Parser.mapError
    (\input -> const . UnexpectedError $ "Could not parse literal from " ++ show input)
    ( do
        ignore whitespace
        output <- Parser.choice [intL, stringL, identifierL]
        ignore whitespace
        return output
    )

stringL :: (Stream stream Char, Show stream) => Lexer stream
stringL =
  do
    skip "\""
    s <- while (/= '\"')
    return $ LiteralT $ StringL s

identifierL :: (Stream stream Char, Show stream) => Lexer stream
identifierL =
  do
    first <- letter
    rest <- many (letter `or` digit)
    return $ LiteralT $ IdentifierL (first : rest)

intL :: (Stream stream Char, Show stream) => Lexer stream
intL =
  do
    ds <- some digit
    LiteralT . IntL <$> Parser.liftResult (Result.read (UnexpectedError $ "Could not parse integer from" ++ ds) ds)

digit :: (Stream stream Char) => Parser stream Char Char
digit = oneOf ['0' .. '9']

letter :: (Stream stream Char) => Parser stream Char Char
letter = oneOf ['a' .. 'z'] <|> oneOf ['A' .. 'Z']

whitespace :: (Stream stream Char) => Parser stream Char String
whitespace = while (== ' ')