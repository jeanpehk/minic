module Parser where

import AST

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- This module contains the entire Parser for minic.

-- The Parser assumes no white space or comments before tokens, and consumes
-- all white space and comments after it.

type Parser = Parsec Void String

ichars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
digits = ['0'..'9']

-- keywords reserved by the language
keywords = ["auto", "break", "case", "char", "const", "continue", "default"
            , "do", "double", "else", "enum", "extern", "float", "for"
            , "goto", "if", "int", "long", "register", "return", "short"
            , "signed", "sizeof", "static", "struct", "switch", "typedef"
            , "union", "unsigned", "void", "volatile", "while"]


-- Skips all white space characters and commented sections
skip :: Parser ()
skip = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- Helper to clear trailing white space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skip

-- Match given string
symbol :: String -> Parser String
symbol = L.symbol skip

int :: Parser Int
int = lexeme L.decimal

parseVariable :: Parser Expr
parseVariable = Var <$> lexeme identifier

parseConst :: Parser Expr
parseConst = IntConst <$> int

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parses a unit with the smallest precedence in an expression
parseTerm :: Parser Expr
parseTerm = parens parseExpr
         <|> parseVariable
         <|> parseConst


-- Operator table
-- Every inner list is a level of precedence that contains
-- all the operators of that level.
-- Precedence levels are in descending order.
-- Assignment needs to handle context later on.
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ binary "*" Mul
    , binary "/" Div],
    [ binary "+" Add
    , binary "-" Subtr],
    [ binary "<" Lt
    , binary ">" Gt
    , binary "==" Eq]
  ]

-- Helper to write binops for op table
binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary op f = InfixL $ f <$ symbol op

-- Parses an expression using Megaparsec's `makeExprParser`,
-- parseTerm and operatorTable
parseExpr :: Parser Expr
parseExpr =  makeExprParser parseTerm operatorTable

-- Parser for a single translation unit (i.e, a single source code file)
tunit :: Parser TUnit
tunit = undefined

-- Parser for types
tpe :: Parser Type
tpe =  CInt  <$ chunk "int"
   <|> CVoid <$ chunk "void"

-- Parses an identifier
-- Also needs to check that it is not a reserved keyword
identifier :: Parser Id
identifier = (lexeme . try) (lxm >>= check)
    where
     lxm = ((:) <$> ((oneOf ichars)) <*> (many (oneOf digits <|> oneOf ichars)))
     check x =
      if x `elem` keywords
      then fail $ show x ++ " is a keyword, not an identifier"
      else return x

