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

-- Match single char

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
    , binary "==" Eq
    , assign "=" Assign]
  ]

-- Helper to write binops for op table
binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary op f = InfixL $ f <$ symbol op

-- Helper to write assign stmt for op table
assign :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
assign op f = InfixR $ f <$ symbol op

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

-- A single starting char for an identifier, i.e "a-zA-Z_"
idStart :: Parser Char
idStart = lexeme lowerChar
     <|>  lexeme upperChar
     <|>  lexeme (char '_')

-- A single id char that does not need to start the id, i.e "a-zA-Z_0-9"
idRest :: Parser Char
idRest =  idStart
      <|> lexeme digitChar

-- Parses an identifier
-- Also needs to check that it is not a reserved keyword
identifier :: Parser Id
identifier = (lexeme . try) (lxm >>= check)
    where
     lxm = ((:) <$> idStart <*> many idRest)
     check x =
      if x `elem` keywords
      then fail $ show x ++ " is a keyword, not an identifier"
      else return x

