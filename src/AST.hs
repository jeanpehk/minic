module AST where

-- Datatypes for the parser to use
-- to construct the AST

-- Translation unit
newtype TUnit
  = TUnit [TL]
  deriving (Eq, Show)

-- Top-level declarations / definitions
data TL
  = GDecl Decl
  | FDef Func
  deriving (Eq, Show)

-- Function
data Func
  = Func Type Id [Param] Block
  deriving (Eq, Show)

-- Program block, i.e a compound statement
newtype Block
  = Block [Either Decl Stmt]
  deriving (Eq, Show)

data Decl
  = Decl Type Id
  deriving (Eq, Show)

data Param
  = Param Type Id
  deriving (Eq, Show)

data Stmt
  = BlockStmt Block
  | ExprStmt Expr
  | IfElse Expr Stmt Stmt
  | While Expr Stmt
  | Null
  deriving (Eq, Show)

data Expr
  = Var Id
  | IntConst Int
  | Add Expr Expr
  | Subtr Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Eq Expr Expr
  | Assign Id Expr
  deriving (Eq, Show)

data Type
  = CInt
  | CVoid
  deriving (Eq, Show)

type Id
  = String

