{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

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

data Token
  = OpenParenthesisT
  | CloseParenthesisT
  | OpenBracketT
  | CloseBracketT
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
  = IfKW
  | ThenKW
  | ElseKW
  | LetKW
  | InKW
  | TypeKW
  deriving (Show, Eq)

data Literal
  = IntL Integer
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

nonLiteral :: (Stream stream Maybe Char, Monad m) => Parser stream Char m Token
nonLiteral =
  nonLiteral'
    [ ("if", KeywordT IfKW),
      ("else", KeywordT ElseKW),
      ("then", KeywordT ThenKW),
      ("let", KeywordT LetKW),
      ("in", KeywordT InKW),
      ("type", KeywordT TypeKW),
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
      ("[", OpenBracketT),
      ("]", CloseBracketT),
      ("-", MinusT),
      ("~", TildeT),
      ("+", PlusT),
      ("*", AsteriskT),
      ("/", DivisionT),
      (":", ColonT),
      (",", CommaT)
    ]
  where
    nonLiteral' s2t =
      case s2t of
        [] -> Parser.fail $ UnexpectedError "Could not parse non literal from token"
        (str, t) : rest ->
          Parser.choice [lit str t, nonLiteral' rest]

    lit str t = do
      skip str
      return t

literal :: (Stream stream Maybe Char, Show stream, Monad m) => Parser stream Char m Token
literal =
  Parser.mapError
    (const . UnexpectedError . ("Could not parse literal from " ++) . show)
    $ Parser.choice [intL, stringL, identifierL]

stringL :: (Stream stream Maybe Char, Show stream, Monad m) => Parser stream Char m Token
stringL =
  do
    skip "\""
    s <- many $ noneOf "\""
    skip "\""
    return $ LiteralT $ StringL s

identifierL :: (Stream stream Maybe Char, Show stream, Monad m) => Parser stream Char m Token
identifierL =
  do
    first <- letter
    rest <- many (letter `or` digit)
    return $ LiteralT $ IdentifierL (first : rest)

intL :: (Stream stream Maybe Char, Show stream, Monad m) => Parser stream Char m Token
intL = do
  ds <- some digit
  result <- Parser.liftResult (Result.read (UnexpectedError $ "Could not parse integer from" ++ ds) ds)
  return $ LiteralT $ IntL result

digit :: (Stream stream Maybe Char, Monad m) => Parser stream Char m Char
digit = oneOf ['0' .. '9']

letter :: (Stream stream Maybe Char, Monad m) => Parser stream Char m Char
letter = oneOf ['a' .. 'z'] <|> oneOf ['A' .. 'Z']

spaces :: (Stream stream Maybe Char, Monad m) => Parser stream Char m String
spaces = while (== ' ')

newLine :: (Stream stream Maybe Char, Monad m) => Parser stream Char m String
newLine = exact "\n"

singleLineComment :: (Stream stream Maybe Char, Monad m) => Parser stream Char m String
singleLineComment = do
  _ <- exact "//"
  while (/= '\n')

token :: (Stream stream Maybe Char, Show stream, Monad m) => Parser stream Char m Token
token =
  Parser.choice
    [ continue,
      nonLit,
      lit
    ]
  where
    continue = do
      ignore (Parser.choice [newLine, singleLineComment])
      token
    nonLit = do
      ignore spaces
      output <- nonLiteral
      ignore spaces
      return output
    lit =
      do
        ignore spaces
        output <- literal
        ignore spaces
        return output

tokens :: (Stream stream Maybe Char, Show stream, Monad m) => Parser stream Char m [Token]
tokens = many token