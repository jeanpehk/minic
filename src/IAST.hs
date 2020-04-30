module IAST where

import AST

-- Internal representation for checked AST

newtype ITUnit
  = ITUnit [ITL]
  deriving (Eq, Show)

data ITL
  = IGlobal IDecl
  | IFDef IFunc
  deriving (Eq, Show)

data IFunc
  = IFunc Type Id [IParam] IBlock
  deriving (Eq, Show)

newtype IBlock
  = IBlock { getIBlock :: [Either IDecl IStmt] }
  deriving (Eq, Show)

data IDecl
  = IDecl Type Id
  deriving (Eq, Show)

data IParam
  = IParam { getIT :: Type, getIPId :: Id }
  deriving (Eq, Show)

data IStmt
  = IBlockS IBlock
  | IExprS IExpr
  | IIfElse IExpr IStmt IStmt
  | IWhile IExpr IStmt
  | IReturn (Maybe IExpr)
  | IPrint IExpr
  | INull
  deriving (Eq, Show)

type IExpr = (IExpr', Type)

data IExpr'
  = IVar Id
  | IVarA Id [Index] -- Array variables
  | IIConst Int
  | ICConst Char
  | IBinOp Op IExpr IExpr
  | IAssign Lvalue IExpr
  | IFCall Id [IExpr]
  deriving (Eq, Show)

-- Lvalues for expressions
data Lvalue
  = IId Id         -- Just an identifier
  | IAId Id [Index]  -- Index of an array
  deriving (Eq, Show)

type Index = Int

