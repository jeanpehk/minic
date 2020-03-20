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
keywords = ["auto", "break", "case", "char", "constant", "continue", "default"
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

-- Parses a single variable
variable :: Parser Expr
variable = Var <$> identifier

-- Parses a constant
constant :: Parser Expr
constant = IntConst <$> int

-- Helper to parse something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parses a unit with the smallest precedence in an expression
term :: Parser Expr
term = parens expr
         <|> variable
         <|> constant

-- Helper to write binops for op table
binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary op f = InfixL $ f <$ symbol op

-- Operator table
-- Every inner list is a level of precedence that contains
-- all the operators of that level.
-- Precedence levels are in descending order.
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [
    [ binary "*" Mul
    , binary "/" Div],
    [ binary "+" Add
    , binary "-" Subtr],
    [ binary "<" Lt
    , binary ">" Gt],
    [binary "==" Eq]
  ]

-- Parser an lvalue for assignment expressions
lvalue :: Parser Id
lvalue = (lexeme . try) (identifier <* (char '=') <* notFollowedBy (char '='))

-- Parser for a single translation unit (i.e, a single source code file)
-- Parses multiple top level declarations and definitions
-- tunit : topLevel* ;
tunit :: Parser TUnit
tunit = TUnit <$> many topLevel

-- Parser for top-level declarations and definitions
-- topLevel : decl
--          | func ;
topLevel :: Parser TL
topLevel =  try (GDecl <$> decl)
        <|> FDef <$> func

-- Parser for basic declarations
-- decl : type id ';' ;
decl :: Parser Decl
decl = Decl <$> tpe <*> (identifier <* lexeme (char ';'))

-- Parser for functions
-- func : type id params block ;
func :: Parser Func
func = Func <$> tpe <*> identifier <*> params <*> block

-- Parser for parameters
-- params : param (',' param)*
params :: Parser [Param]
params = do
  lexeme (char '(')
  ps <- param `sepBy` lexeme (char ',')
  lexeme (char ')')
  return $ ps

-- Parses a single param
-- param : type id ;
param :: Parser Param
param = Param <$> tpe <*> identifier

-- Parses an expression
-- expr : id '=' arithExpr
--      | arithExpr
expr :: Parser Expr
expr = Assign <$> lvalue <*> expr
    <|> exprArith

-- Parses an arithmetic expression using Megaparsec's `makeExprParser`
exprArith :: Parser Expr
exprArith = makeExprParser term operatorTable

-- Parses a single program block
-- block : '{' (stmt | decl)* '}' ;
block :: Parser Block
block = do
  lexeme (char '{')
  list <- many (Left <$> decl <|> Right <$> stmt)
  lexeme (char '}')
  return $ Block list

-- Parses a single statement
-- stmt : block
--      | exprStmt
--      | ifElse
--      | while ;
stmt :: Parser Stmt
stmt =  BlockStmt <$> block
    <|> exprStmt
    <|> ifElse
    <|> while
    <|> nullStmt

-- Parses and expression statement
-- expr_stmt : expr ';' ;
exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> (expr <* lexeme (char ';'))

-- Parses an if else statement
-- ifelse : 'if' expr stmt 'else' stmt ;
ifElse :: Parser Stmt
ifElse = do
  symbol "if"
  e <- expr
  s1 <- stmt
  symbol "else"
  s2 <- stmt
  return $ IfElse e s1 s2

-- Parses a while statement
-- while : 'while' '(' expr ')' stmt ;
while :: Parser Stmt
while = do
  symbol "while"
  e <- parens expr
  s <- stmt
  return $ While e s

-- Parses a null statement
-- nullStmt : ';' ;
nullStmt :: Parser Stmt
nullStmt = Null <$ (lexeme (char ';'))

-- Parses a type
-- type : 'int'
--      | 'void' ;
tpe :: Parser Type
tpe =  CInt  <$ lexeme (chunk "int")
   <|> CVoid <$ lexeme (chunk "void")

-- Parses a starting character of an identifier
idStart :: Parser Char
idStart = lexeme lowerChar
     <|>  lexeme upperChar
     <|>  lexeme (char '_')

-- Parses a character of an identifier that does not
-- need to start the identifier
idRest :: Parser Char
idRest =  idStart
      <|> lexeme digitChar

-- Parses an identifier
-- Also checks that it is not a reserved keyword
-- identifier : [a-zA-Z_] [a-zA-Z_0-9]* ;
identifier :: Parser Id
identifier = (lexeme . try) (lxm >>= check)
    where
     lxm = ((:) <$> idStart <*> many idRest)
     check x =
      if x `elem` keywords
      then fail $ show x ++ " is a keyword, not an identifier"
      else return x

