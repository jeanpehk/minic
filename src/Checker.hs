{-# LANGUAGE RecursiveDo #-}

module Checker where

import AST
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- Type and context checking for minic.

------------------------------------------------------------
-- Datatypes
------------------------------------------------------------

type Checker a = ExceptT Error (State Env) a

data SymbolTable = ST { getSt :: Map.Map Id Type }
  deriving (Eq, Show)

-- Type environment for the program
data Env = Env { active :: SymbolTable
               , blocks :: [SymbolTable]
               , funcs  :: Map.Map Id Type
               , used   :: [SymbolTable] }
  deriving (Eq, Show)

-- Error datatypes
data Error
  = TError String            -- General type errors
  | SError String            -- Syntax errors
  deriving (Eq, Show)

------------------------------------------------------------
-- Helper functions for Checker
------------------------------------------------------------

-- Error text for declaration errors
dError :: Id -> String
dError id = "id: '" ++ id ++ "' already declared"

-- Adds a new block into Env and makes it active
addBlock :: Env -> Env
addBlock (Env active blocks funcs used) = Env (ST Map.empty) (active:blocks)
                                              funcs used

-- Drops the current active block active ones
dropBlock :: Env -> Env
dropBlock env = case blocks env of
                  (x:xs) -> Env { active = x, blocks = xs
                                , funcs = funcs env
                                , used   = active env:used env }
                  []     -> Env { active = ST Map.empty, blocks = []
                                , funcs = funcs env
                                , used   = used env }

-- Look for id from blocks incase it is not in the active one
lookFromBlocks :: Id -> [SymbolTable] -> Maybe Type
lookFromBlocks id (x:xs) = case Map.lookup id (getSt x) of
                             Nothing -> lookFromBlocks id xs
                             Just x  -> Just x
lookFromBlocks id []  = Nothing

-- Look for id from active symbol table
-- Used for declaring values
getId :: Id -> Env -> Maybe Type
getId id env = Map.lookup id (getSt (active env))

-- Get types of already declared variables from the entire Env
getDeclaredId :: Id -> Env -> Maybe Type
getDeclaredId id env = case Map.lookup id st of
                 Nothing -> case lookFromBlocks id (blocks env) of
                              Nothing -> Map.lookup id (funcs env)
                              Just x  -> Just x
                 Just x  -> Just x
  where
    st = getSt (active env)

-- Look for id from functions
-- Used for declaring or calling functions
getFunc :: Id -> Env -> Maybe Type
getFunc id env = case Map.lookup id (funcs env) of
                   Nothing -> Map.lookup id $ getSt (active env)
                   Just x  -> Just x

-- Add a new identifier into the environment
addId :: Type -> Id -> Env -> Env
addId tpe id env = Env { active = ST (Map.insert id tpe (getSt (active env)))
                            , blocks = blocks env
                            , funcs  = funcs env
                            , used   = used env }

addFunc :: Type -> Id -> Env -> Env
addFunc tpe id env = Env { active = active env
                         , blocks = blocks env
                         , funcs = Map.insert id tpe (funcs env)
                         , used = used env}

------------------------------------------------------------
-- Type comparisons for expressions
------------------------------------------------------------

compareTypes :: Type -> Type -> Either Error Type
compareTypes (CInt) (CInt) = Right CInt
compareTypes (CChar) (CChar) = Right CInt
compareTypes (CChar) (CInt) = Right CInt
compareTypes (CInt) (CChar) = Right CInt
compareTypes (Pntr a) (Pntr b) =
  case compareTypes a b of
    Left err -> Left err
    Right t  -> Right $ Pntr t
compareTypes _ (Pntr _) = Left $ TError "Cannot combine ptr with non ptr"
compareTypes (Pntr _) _ = Left $ TError "Cannot combine ptr with non ptr"
compareTypes (CVoid) _ = Left $ TError "Cannot combine Void with another type"
compareTypes _ (CVoid) = Left $ TError "Cannot combine Void with another type"

------------------------------------------------------------
-- Check program
------------------------------------------------------------

runChecker :: TUnit -> (Either Error TUnit, Env)
runChecker tunit = (runState . runExceptT) (checkTu tunit) env
  where
    env = Env { active = ST Map.empty, blocks = [], funcs = Map.empty, used = [] }

------------------------------------------------------------
-- Translation unit
------------------------------------------------------------

checkTu :: TUnit -> Checker TUnit
checkTu (TUnit tl) = do
  tls <- mapM checkTl tl
  return $ TUnit tls

------------------------------------------------------------
-- Top-level
------------------------------------------------------------

checkTl :: TL -> Checker TL
checkTl (GDecl (Decl CVoid _)) = throwError $ SError "Void cannot have an identifier"
checkTl gd@(GDecl (Decl tpe id)) = do
  env <- get
  case getId id env of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ TError (dError id)
  return gd

------------------------------------------------------------
-- Function
------------------------------------------------------------

checkTl (FDef (Func tpe id params block)) = mdo
  env <- get
  -- Add id first into functions in env
  case getFunc id env of
    Nothing -> put $ addFunc tpe id env
    Just x  -> throwError $ TError (dError id)
  nenv <- get
  put $ addBlock nenv
  ps <- mapM checkParam params
  let f = \y -> case y of
            Left d  -> do
                        decl <- checkDecl d
                        return $ Left decl
            Right s -> do
                        stmt <- checkStmt s
                        return $ Right stmt
  bls <- mapM f (getBlock block)
  nnenv <- get
  put $ dropBlock nnenv
  return $ FDef (Func tpe id ps (Block bls))

------------------------------------------------------------
-- Parameters
------------------------------------------------------------

checkParam :: Param -> Checker Param
checkParam (Param CVoid _) = throwError $ TError "Void param cannot have an identifier"
checkParam p@(ParamNoId CVoid) = return p
checkParam (ParamNoId tpe  ) = throwError $ TError "Non-void param needs an identifier"
checkParam p@(Param tpe id) = do
  env <- get
  case getId id env of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ TError (dError id)
  return p

------------------------------------------------------------
-- Block
------------------------------------------------------------

checkBlock :: Block -> Checker Block
checkBlock (Block ds) = do
  env <- get
  put $ addBlock env
  let f = \y -> case y of
             Left d  -> do
                         decl <- checkDecl d
                         return $ Left decl
             Right s -> do
                         stmt <- checkStmt s
                         return $ Right stmt
  get
  bls <- mapM f ds
  nnenv <- get
  put $ dropBlock nnenv
  return $ Block bls

------------------------------------------------------------
-- Statements
------------------------------------------------------------

-- Block
checkStmt :: Stmt -> Checker Stmt
checkStmt (BlockStmt b) = do
  bl <- checkBlock b
  return $ BlockStmt bl

-- Expression
checkStmt (ExprStmt e) = do
  ce <- checkExpr e
  return $ ExprStmt (fst ce)

-- IfElse
checkStmt (IfElse e1 s1 s2) = do
  ce <- checkExpr e1
  cs1 <- checkStmt s1
  cs2 <- checkStmt s2
  return $ IfElse (fst ce) cs1 cs2

-- While
checkStmt (While e s) = do
  ce <- checkExpr e
  cs <- checkStmt s
  return $ While (fst ce) cs

-- Return
checkStmt (Return e) = do
  case e of
    Nothing -> return $ Return Nothing
    Just x  -> do
                ce <- checkExpr x
                return $ Return $ Just (fst ce)

-- Print
checkStmt (Print e) = do
  ce <- checkExpr e
  case snd ce of
    CInt -> return $ Print e
    _    -> throwError $ TError ("Only int print's allowed at the moment")

-- Null
checkStmt Null = return Null

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

checkDecl :: Decl -> Checker Decl
checkDecl (Decl CVoid _ ) = throwError $ SError "Void cannot have an identifier"
checkDecl d@(Decl tpe id) = do
  env <- get
  case getId id env of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ TError (dError id)
  return d

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

-- Variable
checkExpr :: Expr -> Checker (Expr, Type)
checkExpr e@(Var id) = do
  env <- get
  case getDeclaredId id env of
    Nothing -> throwError $ TError (dError id)
    Just x  -> return (e, x)

-- Constants
checkExpr e@(IntConst i)  = return (e, CInt)
checkExpr e@(CharConst c) = return (e, CChar)

-- BinOps
checkExpr e@(BinOp op e1 e2) = do
  ce1 <- checkExpr e1
  ce2 <- checkExpr e2
  case compareTypes (snd ce1) (snd ce2) of
    Left err -> throwError err
    Right x  -> case x of
                  -- Change type of BinOp if needed so
                  -- we know when coercions are needed
                  CInt -> return (e, x)
                  _    -> throwError $ SError "Only Int BinOps supported"

-- Assignment
checkExpr e@(Assign id expr) = do
  env <- get
  case getDeclaredId id env of
    Nothing -> throwError $ TError ("Var not declared: " ++ id)
    Just x  -> do
      etype <- checkExpr expr
      case compareTypes x (snd etype) of
        Left err -> throwError err
        Right x  -> return (e, x)

-- Function calls
checkExpr e@(FCall id) = do
  env <- get
  case getFunc id env of
    Nothing -> throwError $ TError ("Function: " ++ id ++ " not declared")
    Just x  -> return (e, x)

