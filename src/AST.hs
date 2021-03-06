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
  = Func { getFT :: Type
         , getFId ::  Id
         , getFP :: [Param]
         , getFB ::  Block }
  deriving (Eq, Show)

-- Program block, i.e a compound statement
newtype Block
  = Block { getBlock :: [Either Decl Stmt] }
  deriving (Eq, Show)

data Decl
  = Decl Type Id
  deriving (Eq, Show)

data Param
  = Param { getType :: Type, getPID :: Id}
  | ParamNoId Type
  deriving (Eq, Show)

data Stmt
  = BlockStmt Block
  | ExprStmt Expr
  | IfElse Expr Stmt Stmt
  | While Expr Stmt
  | Return (Maybe Expr)
  | Print Expr
  | Null
  deriving (Eq, Show)

data Expr
  = Var Id
  | VarArr Id [Int] -- Array variables
  | IntConst Int
  | CharConst Char
  | BinOp Op Expr Expr
  | Assign Expr Expr
  | FCall Id [Expr]
  deriving (Eq, Show)

data Op
  = Add
  | Subtr
  | Mul
  | Div
  | Lt
  | Gt
  | Eq
  deriving (Eq, Show)

data Type
  = CInt
  | CChar
  | CVoid
  | Pntr Type
  | Array Int Type
  deriving (Eq, Show)

type Id
  = String

