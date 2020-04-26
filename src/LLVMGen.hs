{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module LLVMGen where

import qualified AST as Mc
import LLVMEnv

import Data.Char
import Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as L
import Data.String
import Data.Word
import Control.Monad.State
import qualified Data.Map as Map

import LLVM.Pretty
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Name as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.IRBuilder.Constant as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module as IR
import qualified LLVM.IRBuilder.Monad as IR

-- Construct the Haskell LLVM AST representation.

------------------------------------------------------------
-- LLVM AST Generation
------------------------------------------------------------

runGen :: String -> Mc.TUnit -> (AST.Module, Env)
runGen nm tunit = runState (IR.buildModuleT (fromString nm) (codeGen tunit)) env
  where
    env = Env { active = ST Map.empty, rest = []
              , externs = Map.empty
              , funcs = Map.empty
              , funcHasRet = False }

------------------------------------------------------------
-- Translation units
------------------------------------------------------------

codeGen :: Mc.TUnit -> Generator ()
codeGen (Mc.TUnit tls) = do
  prnt <- IR.extern (AST.mkName "print") [AST.i32] AST.void
  addExtern prnt
  let x = \y -> case y of
                  (Mc.GDecl decl) -> genGlobalDecl decl
                  (Mc.FDef func)  -> genFunc func
  mapM_ x tls

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

genGlobalDecl :: (MonadState Env m, IR.MonadModuleBuilder m)
        => Mc.Decl
        -> m ()
genGlobalDecl (Mc.Decl Mc.CVoid id) = error "Can't have void decl with id"
genGlobalDecl (Mc.Decl tpe id) = do
  env <- get
  d <- IR.global (AST.mkName id) (decideType tpe) (AST.Int (fromIntegral 32) 0)
  addToActive id d
  return ()


genDecl :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.Decl
        -> m ()
genDecl (Mc.Decl tpe id) = do
  env <- get
  d <- case tpe of
    Mc.CInt   -> IR.alloca AST.i32 Nothing 4
    Mc.CChar  -> IR.alloca AST.i8 Nothing 1
    Mc.CVoid  -> error "Can't have void decl with id"
    Mc.Pntr p -> IR.alloca (decideType (Mc.Pntr p)) Nothing 8
  addToActive id d
  return ()

------------------------------------------------------------
-- Functions
------------------------------------------------------------

genFunc :: (MonadState Env m, IR.MonadModuleBuilder m, MonadFix m)
        => Mc.Func
        -> m ()
genFunc (Mc.Func tpe id params block) = mdo
  let ps = map mkParam params
  -- Insert function to env using mdo to allow recursive calls
  addFunc id f

  f <- IR.function (AST.mkName id) ps (decideType tpe) $ \ops -> do
    newActive
    -- Add params to symboltable
    paramsToEnv params ops
    -- generate params
    gparams <- mapM genParam [(p, o) | p <- params | o <- ops]
    -- add new generated params to env
    paramsToEnv params gparams

    let f = \y -> case y of
                    Left decl  -> genDecl decl
                    Right stmt -> genStmt stmt

    mapM_ f $ Mc.getBlock block

    -- Check whether return stmt is already generated
    envAfter <- get
    case funcHasRet envAfter of
      False -> IR.ret $ IR.int32 0
      True  -> modify $ \e -> e { funcHasRet = False }
  dropActive
  return ()

------------------------------------------------------------
-- Statements
------------------------------------------------------------

-- Block statements
genStmt :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m, MonadFix m)
        => Mc.Stmt
        -> m ()
genStmt (Mc.BlockStmt bl) = do
  newActive
  let f = \y -> case y of
                  Left decl  -> genDecl decl
                  Right stmt -> genStmt stmt
  mapM_ f $ Mc.getBlock bl
  dropActive
  return ()

-- Expr statements
genStmt (Mc.ExprStmt expr) = do
  genExpr expr
  return ()

-- If Else statements
genStmt (Mc.IfElse expr stmt1 stmt2) = mdo
  e <- genExpr expr
  IR.condBr e thn els

  -- Then
  thn <- IR.block
  s1 <- genStmt stmt1
  IR.br continue
  -- Else
  els <- IR.block
  s2 <- genStmt stmt2
  IR.br continue

  continue <- IR.block
  return ()

-- While statements
genStmt (Mc.While expr stmt) = mdo
  -- Starting block with comparison
  IR.br bl
  bl <- IR.block
  e <- genExpr expr
  IR.condBr e sBlock continue

  -- Statement block
  sBlock <- IR.block
  s <- genStmt stmt
  IR.br bl

  -- Continuing block
  continue <- IR.block
  return ()

-- Return statements
genStmt (Mc.Return x) = do
  modify $ \env -> env { funcHasRet = True }
  case x of
    Nothing -> do
                IR.retVoid
    Just x  -> do
                e <- genExpr x
                IR.ret e

-- Print statements
genStmt (Mc.Print expr) = do
  e <- genExpr expr
  env <- get
  let op = getPrint env
  IR.call op [(e, [])]
  return ()

-- Null statements
genStmt (Mc.Null) = return ()

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

-- Variables
genExpr :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.Expr
        -> m AST.Operand
genExpr (Mc.Var id) = do
  env <- get
  let op = idFromEnv id env
  IR.load op 8

-- Int Constants
genExpr (Mc.IntConst int) = return $ constInt32 int

-- Char Constants
genExpr (Mc.CharConst char) = return $ IR.int8 $ toInteger $ ord char

-- BinOps
genExpr (Mc.BinOp op e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  case op of
    Mc.Add   -> IR.add e1 e2
    Mc.Subtr -> IR.sub e1 e2
    Mc.Mul   -> IR.mul e1 e2
    Mc.Div   -> IR.sdiv e1 e2
    Mc.Lt    -> IR.icmp AST.SLT e1 e2
    Mc.Gt    -> IR.icmp AST.SGT e1 e2
    Mc.Eq    -> IR.icmp AST.EQ e1 e2

-- Assignments
genExpr (Mc.Assign id expr) = do
  env <- get
  let var = idFromEnv id env
  e <- genExpr expr
  IR.store var 8 e
  return var

-- Function calls
genExpr (Mc.FCall id args) = do
  env <- get
  as <- mapM genExpr args
  let mkArgs = fmap (\y -> (y, [])) as
  IR.call (funcFromEnv id env) mkArgs

------------------------------------------------------------
-- Params
------------------------------------------------------------

genParam :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => (Mc.Param, AST.Operand)
        -> m AST.Operand
genParam ((Mc.Param _ id), (AST.LocalReference tpe nm)) = do
  env <- get
  let op = idFromEnv id env
  addr <- IR.alloca tpe Nothing 8
  IR.store addr 8 op
  return addr
genParam _ = error "There should be only local refs in params"

