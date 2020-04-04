module Checker where

import AST
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- This module contains type and context checking for minic.

data SymbolTable = ST { getSt :: Map.Map Id Type }
  deriving (Eq, Show)

-- Type environment for the program
data Env = Env { active :: SymbolTable
               , blocks :: [SymbolTable]
               , used   :: [SymbolTable] }
  deriving (Eq, Show)

-- Error datatypes
data Error
  = TError String            -- General type errors
  | SError String            -- Syntax errors
  deriving (Eq, Show)

-- Error text for declaration errors
dError :: Id -> String
dError id = "id: '" ++ id ++ "' already declared"

-- Datatype for the checker
-- Except for error handling and
-- State to contain the environment
type Checker a = ExceptT Error (State Env) a

-- Run the checker
runChecker :: TUnit -> (Either Error (), Env)
runChecker tunit = (runState . runExceptT) (checkTu tunit) env
  where
    env = Env { active = ST Map.empty, blocks = [], used = [] }

-- Add a new identifier into the environment
addId :: Type -> Id -> Env -> Env
addId tpe id env = Env { active = ST (Map.insert id tpe (getSt (active env)))
                            , blocks = blocks env
                            , used   = used env }

-- Adds a new block into Env and makes it active
addBlock :: Env -> Env
addBlock (Env active blocks used) = Env (ST Map.empty) (active:blocks) (used)

-- Drops the current active block active ones
dropBlock :: Env -> Env
dropBlock env = case blocks env of
                  (x:xs) -> Env { active = x, blocks = xs
                                , used   = active env:used env }
                  []     -> Env { active = ST Map.empty, blocks = []
                                , used   = used env }

-- Look for id from blocks incase it is not in the active one
lookFromBlocks :: Id -> [SymbolTable] -> Maybe Type
lookFromBlocks id (x:xs) = case Map.lookup id (getSt x) of
                             Nothing -> lookFromBlocks id xs
                             Just x  -> Just x
lookFromBlocks id []  = Nothing

-- Get types of already declared variables from the entire Env
getDeclaredId :: Id -> Env -> Maybe Type
getDeclaredId id env = case Map.lookup id st of
                 Nothing -> lookFromBlocks id (blocks env)
                 Just x  -> Just x
  where
    st = getSt (active env)

-- Look for id from active symbol table
-- Used for declaring values
getId :: Id -> Env -> Maybe Type
getId id env = Map.lookup id (getSt (active env))

-- Check translation unit
checkTu :: TUnit -> Checker ()
checkTu (TUnit tl) = do
  mapM checkTl tl
  return ()

-- Check top-level
checkTl :: TL -> Checker ()
checkTl (GDecl (Decl CVoid _)) = throwError $ SError "Void cannot have an identifier"
checkTl (GDecl (Decl tpe id)) = do
  env <- get
  case getId id env of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ TError (dError id)
  return ()

-- Check function
checkTl (FDef (Func tpe id params block)) = do
  env <- get
  -- Add id first into top level declarations before creating a new block
  case getId id env of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ TError (dError id)
  nenv <- get
  put $ addBlock nenv
  mapM checkParam params
  let f = \y -> case y of
            Left d  -> checkDecl d
            Right s -> checkStmt s
  mapM f (getBlock block)
  nnenv <- get
  put $ dropBlock nnenv
  return ()

-- Check parameters
checkParam :: Param -> Checker ()
checkParam (Param CVoid _) = throwError $ TError "Void param cannot have an identifier"
checkParam (ParamNoId CVoid) = return ()
checkParam (ParamNoId tpe  ) = throwError $ TError "Non-void param needs an identifier"
checkParam (Param tpe id) = do
  env <- get
  case getId id env of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ TError (dError id)

-- Check a block
checkBlock :: Block -> Checker ()
checkBlock (Block ds) = do
  env <- get
  put $ addBlock env
  let f = \y -> case y of
             Left d  -> checkDecl d
             Right s -> checkStmt s
  get
  mapM f ds
  nnenv <- get
  put $ dropBlock nnenv
  return ()

-- Checks for statements
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

-- Checks for declarations
checkDecl :: Decl -> Checker ()
checkDecl (Decl CVoid _ ) = throwError $ SError "Void cannot have an identifier"
checkDecl (Decl tpe id) = do
  env <- get
  case getId id env of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ TError (dError id)

-- Checks for expressions
checkExpr :: Expr -> Checker Type
checkExpr (Var id) = do
  env <- get
  case getDeclaredId id env of
    Nothing -> throwError $ TError (dError id)
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
  case getDeclaredId id env of
    Nothing -> throwError $ TError ("Var not declared: " ++ id)
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

