module Parser where

import AST

import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

------------------------------------------------------------

-- Parser for minic.

-- The Parser assumes no white space or comments before
-- tokens, and consumes all white space and comments after it.

------------------------------------------------------------

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

-- Match single int
int :: Parser Int
int = lexeme L.decimal

-- Helper to parse something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Helper to parse something between square brackets
squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

-- Parser for a single translation unit (i.e, a single source code file)
-- Parses multiple top level declarations and definitions
-- tunit : topLevel* ;
tunit :: Parser TUnit
tunit = do
  skip
  tls <- many topLevel
  eof
  return $ TUnit tls

-- Parser for top-level declarations and definitions
-- topLevel : decl
--          | func ;
topLevel :: Parser TL
topLevel =  try (GDecl <$> decl)
        <|> try (FDef <$> func)

-- Parser for basic declarations
-- decl : type id ';' ;
decl :: Parser Decl
decl = do
  t <- tpe
  i <- identifier
  -- Add arrays if needed
  tt <- foldr (\e -> Array e) t <$> (many (squares int))
  symbol ";"
  return $ Decl tt i

-- Parser for functions
-- func : type id params block expr?;
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
param = try $ Param <$> tpe <*> identifier
     <|> ParamNoId <$> tpe

-- Parses an expression
-- expr : id '=' arithExpr
--      | arithExpr
expr :: Parser Expr
expr = Assign <$> lvalue <*> expr
    <|> exprArith

-- Parses an arithmetic expression using Megaparsec's `makeExprParser`
exprArith :: Parser Expr
exprArith = makeExprParser term operatorTable

-- Parses a smallest unit in an expression
term :: Parser Expr
term = parens expr
    <|> try fcall
    <|> do
          variable
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
    [ binary "*" (BinOp Mul)
    , binary "/" (BinOp Div)],
    [ binary "+" (BinOp Add)
    , binary "-" (BinOp Subtr)],
    [ binary "<" (BinOp Lt)
    , binary ">" (BinOp Gt)],
    [binary "==" (BinOp Eq)]
  ]

-- Parses an lvalue for assignment expressions
lvalue :: Parser Id
lvalue = (lexeme . try) (identifier <* (char '=') <* notFollowedBy (char '='))

-- Parses a single statement
-- stmt : block
--      | print int
--      | exprStmt
--      | ifElse
--      | while
--      | return
--      | null ;
stmt :: Parser Stmt
stmt =  BlockStmt <$> block
    <|> try prnt
    <|> try exprStmt
    <|> ifElse
    <|> while
    <|> ret
    <|> nullStmt

-- Parses a single program block
-- block : '{' (stmt | decl)* '}' ;
block :: Parser Block
block = do
  lexeme (char '{')
  list <- many (Left <$> decl <|> Right <$> stmt)
  lexeme (char '}')
  return $ Block list

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

-- Parses a return statement
ret :: Parser Stmt
ret = do
  lexeme (chunk "return")
  e <- optional expr
  symbol ";"
  return $ Return e

-- Parses a simplified print statement
-- prnt : 'print' '(' expr ')' ';' ;
prnt :: Parser Stmt
prnt = do
  lexeme (chunk "print")
  e <- parens expr
  symbol ";"
  return $ Print e

-- Parses a null statement
-- nullStmt : ';' ;
nullStmt :: Parser Stmt
nullStmt = Null <$ (lexeme (char ';'))

-- Parses a function call with no args
-- fcall : identifier '(' ')'
fcall :: Parser Expr
fcall = do
  id <- identifier
  symbol "("
  args <- expr `sepBy` (symbol ",")
  symbol ")"
  return $ FCall id args

-- Parses a type
-- type : 'int'
--      | 'void' ;
tpe :: Parser Type
tpe = do
  tp <- pureTpe
  foldr (const Pntr) tp <$> (many (symbol "*"))

-- Types with no pointers
pureTpe :: Parser Type
pureTpe =
  (CInt  <$ lexeme (try ((chunk "int" <* notFollowedBy (idStart <|> idRest)))))
  <|> (CChar <$ lexeme (chunk "char" <* notFollowedBy (idStart <|> idRest)))
  <|> (CVoid <$ lexeme (chunk "void" <* notFollowedBy (idStart <|> idRest)))

-- Parses a single variable
variable :: Parser Expr
variable = try (VarArr <$> identifier <*> squares int)
        <|> Var <$> identifier

-- character constant, e.g 'k'
charConst :: Parser Char
charConst = lexeme $ between (symbol "'") (symbol "'") $ (lowerChar <|> upperChar)

-- Parses a constant
constant :: Parser Expr
constant = IntConst <$> int
        <|> CharConst <$> charConst


-- Parses a starting character of an identifier
idStart :: Parser Char
idStart = lowerChar
       <|>  upperChar
       <|>  (char '_')

-- Parses a character of an identifier that does not
-- need to start the identifier
idRest :: Parser Char
idRest =  idStart
      <|> digitChar

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

