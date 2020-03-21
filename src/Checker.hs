module Checker where

import AST
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- This module contains type and context checking for minic.

data SymbolTable = ST { getSt :: Map.Map Id Type }

-- Type environment for the program
data Env = Env { active :: SymbolTable, blocks :: [SymbolTable] }

-- Error datatypes
data Error
  = TError String            -- General type errors
  | DError Id                -- Declaration errors
  | SError String            -- Syntax errors
  deriving (Eq, Show)

-- Datatype for the checker
-- Except for error handling and
-- State to contain the environment
type Checker a = ExceptT Error (State Env) a

-- Run the checker
runChecker :: TUnit -> (Either Error (), Env)
runChecker tunit = (runState . runExceptT) (checkTu tunit) env
  where
    env = Env { active = ST Map.empty, blocks = [ST Map.empty] }

-- Add a new identifier into the environment
addId :: Type -> Id -> Env -> Env
addId tpe id env = Env { active = ST (Map.insert id tpe (getSt (active env)))
                       , blocks = blocks env }
-- Check translation unit
checkTu :: TUnit -> Checker ()
checkTu (TUnit tl) = do
  mapM checkTl tl
  return ()

-- Check top-level
checkTl :: TL -> Checker ()
checkTl (GDecl d) = checkDecl d

-- Check function
checkTl (FDef (Func tpe id params block)) = do
  env <- get
  case Map.lookup id (getSt (active env)) of
    Nothing -> put $ addId tpe id env -- TODO prop..
    Just x  -> throwError $ TError ("id: '" ++ id ++ "' already declared")
  mapM checkParam params
  checkBlock block
  return ()

-- Check parameters
checkParam :: Param -> Checker ()
checkParam (Param tpe id) = do
  env <- get
  case Map.lookup id (getSt (active env)) of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ DError id

-- Check a block
checkBlock :: Block -> Checker ()
checkBlock (Block ds) = do
  let f = \y -> case y of
             Left d  -> checkDecl d
             Right s -> checkStmt s
  mapM f ds
  return ()

-- Check a statement
checkStmt :: Stmt -> Checker ()
checkStmt (BlockStmt b) = checkBlock b
checkStmt (ExprStmt e) = do
  checkExpr e
  return ()
checkStmt (IfElse e1 s1 s2) = do
  checkExpr e1
  checkStmt s1
  checkStmt s2
  return ()
checkStmt (While e s) = do
  checkExpr e
  checkStmt s
  return ()
checkStmt Null = return ()

-- Check a declaration
checkDecl :: Decl -> Checker ()
checkDecl (Decl CVoid _ ) = throwError $ SError "Void cannot have an identifier"
checkDecl (Decl tpe id) = do
  env <- get
  case Map.lookup id (getSt (active env)) of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ DError id

-- Check an expression TODO prop checks only active block
checkExpr :: Expr -> Checker Type
checkExpr (Var id) = do
  env <- get
  case Map.lookup id (getSt (active env)) of
    Nothing -> throwError $ DError id
    Just x  -> return x

checkExpr (IntConst i)  = return CInt
checkExpr (Subtr e1 e2) = binops e1 e2
checkExpr (Add e1 e2)   = binops e1 e2
checkExpr (Mul e1 e2)   = binops e1 e2
checkExpr (Div e1 e2)   = binops e1 e2
checkExpr (Lt e1 e2)    = binops e1 e2
checkExpr (Gt e1 e2)    = binops e1 e2
checkExpr (Eq e1 e2)    = binops e1 e2
checkExpr (Assign id e) = do
  env <- get
  case Map.lookup id (getSt (active env)) of
    -- TODO prop checks only active block
    Nothing -> throwError $ TError ("id: '" ++ id ++ "' not defined")
    Just x  -> do
      etype <- checkExpr e
      case compareTypes x etype of
        Left err -> throwError err
        Right x  -> return x

-- Helper for checking binops
binops :: Expr -> Expr -> Checker Type
binops e1 e2 = do
  t1 <- checkExpr e1
  t2 <- checkExpr e2
  case compareTypes t1 t2 of
    Left err -> throwError err
    Right x  -> return x

-- Type comparisons
-- If types mismatch coerces them if possible
-- otherwise returns an error.
compareTypes :: Type -> Type -> Either Error Type
compareTypes (CInt) (CInt) = Right CInt
compareTypes (CVoid) _ = Left $ TError "Cannot combine Void with another type"
compareTypes _ (CVoid) = Left $ TError "Cannot combine Void with another type"

