{-# LANGUAGE RecursiveDo #-}

module Checker where

import AST
import IAST
import CheckerEnv
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- Type and context checking for minic.

------------------------------------------------------------
-- Check program
------------------------------------------------------------

runChecker :: TUnit -> (Either Error ITUnit, Env)
runChecker tunit = (runState . runExceptT) (checkTu tunit) env
  where
    env = Env { active = ST Map.empty, blocks = [], funcs = Map.empty, used = [] }

------------------------------------------------------------
-- Translation unit
------------------------------------------------------------

checkTu :: TUnit -> Checker ITUnit
checkTu (TUnit tl) = do
  tls <- mapM checkTl tl
  return $ ITUnit tls

------------------------------------------------------------
-- Top-level
------------------------------------------------------------

checkTl :: TL -> Checker ITL
checkTl (GDecl (Decl CVoid _)) = throwError $ SError "Void cannot have an identifier"
checkTl (GDecl (Decl tpe id)) = do
  env <- get
  case getId id env of
    Nothing -> addId tpe id
    Just x  -> throwError $ TError (dError id)
  return $ IGlobal (IDecl tpe id)

------------------------------------------------------------
-- Function
------------------------------------------------------------

checkTl (FDef (Func tpe id params block)) = mdo
  env <- get
  -- Add id first into functions in env
  case getFunc id env of
    Nothing -> addFunc tpe params id
    Just x  -> throwError $ TError (dError id)
  addBlock
  ps <- mapM checkParam params
  cps <- checkVoids ps
  let f = \y -> case y of
            Left d  -> do
                        decl <- checkDecl d
                        return $ Left decl
            Right s -> do
                        stmt <- checkStmt s
                        return $ Right stmt
  bls <- mapM f (getBlock block)
  dropBlock
  return $ IFDef $ IFunc tpe id cps (IBlock bls)

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
    Nothing -> addId tpe id
    Just x  -> throwError $ TError (dError id)
  return p

checkVoids :: [Param] -> Checker [IParam]
checkVoids [(ParamNoId CVoid)] = return []
checkVoids xs = case (ParamNoId CVoid) `elem` xs of
                  False -> return $ map (\y -> IParam (getType y) (getPID y)) xs
                  True  -> throwError $ TError "Void must be the only param"
------------------------------------------------------------
-- Block
------------------------------------------------------------

checkBlock :: Block -> Checker IBlock
checkBlock (Block ds) = do
  env <- get
  addBlock
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
  dropBlock
  return $ IBlock bls

------------------------------------------------------------
-- Statements
------------------------------------------------------------

-- Block
checkStmt :: Stmt -> Checker IStmt
checkStmt (BlockStmt b) = do
  bl <- checkBlock b
  return $ IBlockS bl

-- Expression
checkStmt (ExprStmt e) = do
  ce <- checkExpr e
  return $ IExprS ce

-- IfElse
checkStmt (IfElse e1 s1 s2) = do
  ce <- checkExpr e1
  cs1 <- checkStmt s1
  cs2 <- checkStmt s2
  return $ IIfElse ce cs1 cs2

-- While
checkStmt (While e s) = do
  ce <- checkExpr e
  cs <- checkStmt s
  return $ IWhile ce cs

-- Return
checkStmt (Return e) = do
  case e of
    Nothing -> return $ IReturn Nothing
    Just x  -> do
                ce <- checkExpr x
                return $ IReturn $ Just ce

-- Print
checkStmt (Print e) = do
  ce <- checkExpr e
  case snd ce of
    CInt -> return $ IPrint ce
    _    -> throwError $ TError ("Only int print's allowed at the moment")

-- Null
checkStmt Null = return INull

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

checkDecl :: Decl -> Checker IDecl
checkDecl (Decl CVoid _)  = throwError $ SError "Void cannot have an identifier"
checkDecl (Decl a@(Array c tpe) id) = do
  case tpe of
    CVoid -> throwError $ TError "Declared array of voids"
    _     -> do
              env <- get
              case getId id env of
                Nothing -> addId a id
                Just x  -> throwError $ TError (dError id)
              return $ IDecl a id
 
checkDecl (Decl tpe id) = do
  env <- get
  case getId id env of
    Nothing -> addId tpe id
    Just x  -> throwError $ TError (dError id)
  return $ IDecl tpe id

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

-- Variable
checkExpr :: Expr -> Checker IExpr
checkExpr (Var id) = do
  env <- get
  case getDeclaredId id env of
    Nothing -> throwError $ TError (dError id)
    Just x  -> return $ (IVar id, x)

-- Array variables
checkExpr (VarArr id inx) = do
  env <- get
  case getDeclaredId id env of
    Nothing -> throwError $ TError (dError id)
    Just x  -> case x of
                Array c tpe -> return (IVarA id inx, x)
                _           -> throwError $ TError "Can only access index of an array"

-- Constants
checkExpr (IntConst i)  = return (IIConst i, CInt)
checkExpr (CharConst c) = return (ICConst c, CChar)

-- BinOps
checkExpr (BinOp op e1 e2) = do
  ce1 <- checkExpr e1
  ce2 <- checkExpr e2
  tpe <- comp (snd ce1) (snd ce2)
  case tpe of
    -- Change type of BinOp if needed so
    -- we know when coercions are needed
    CInt -> return (IBinOp op ce1 ce2, tpe)
    _    -> throwError $ SError "Only Int BinOps supported"

-- Assignment
checkExpr (Assign id expr) = do
  env <- get
  case getDeclaredId id env of
    Nothing -> throwError $ TError ("Var not declared: " ++ id)
    Just x  -> do
      ce <- checkExpr expr
      tpe <- comp x (snd ce)
      return (IAssign id ce, tpe)

-- Function calls
checkExpr (FCall id args) = do
  env <- get
  as <- mapM checkExpr args
  case Map.lookup id (funcs env) of
    Nothing -> throwError $ TError ("Function: " ++ id ++ " not declared")
    Just x  -> do
                fparams (snd x) (fmap snd as)
                return (IFCall id as, fst x)

-- Check function call parameters
fparams :: [Param] -> [Type] -> Checker ()
fparams (x:xs) (y:ys) = do
  tpe <- comp (getType x) y
  fparams xs ys
fparams [] (y:ys) = throwError $ TError "Incorrect amount of args in functions call"
fparams (x:xs) [] = throwError $ TError "Incorrect amount of args in functions call"
fparams [] [] = return ()

------------------------------------------------------------
-- Type comparisons for expressions
------------------------------------------------------------

comp :: Type -> Type -> Checker Type
comp (CInt) (CInt) = return $ CInt
comp (CChar) (CChar) = return $ CInt
comp (CChar) (CInt) = return $ CInt
comp (CInt) (CChar) = return $ CInt
comp (Pntr a) (Pntr b) = do
  r <- comp a b
  return $ Pntr r
comp (Array c1 t1) _ = throwError $ TError "Cannot combine arrays"
comp _ (Array c1 t1) = throwError $ TError "Cannot combine arrays"
comp _ (Pntr _) = throwError $ TError "Cannot combine ptr with non ptr"
comp (Pntr _) _ = throwError $ TError "Cannot combine ptr with non ptr"
comp (CVoid) _ = throwError $ TError "Cannot combine Void with another type"
comp _ (CVoid) = throwError $ TError "Cannot combine Void with another type"

