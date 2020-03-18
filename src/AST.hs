module AST where

-- Datatypes for the parser to use
-- to construct the AST

data TUnit
  = TUnit [Either Decl Func]
  deriving (Eq, Show)

data Func
  = Func Type Id [Param] Block
  deriving (Eq, Show)

data Block
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
  | IfThen Expr Stmt Stmt
  | While Expr Stmt
  deriving (Eq, Show)

data Expr
  = Var Id
  | IntConst Int
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Expr :>: Expr
  | Expr :<: Expr
  | Expr :==: Expr
  | Expr :=: Expr
  deriving (Eq, Show)

data Type
  = CInt
  | CVoid
  deriving (Eq, Show)

type Id
  = String

