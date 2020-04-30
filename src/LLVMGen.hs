{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module LLVMGen where

import AST (Op (..), Type (..))
import qualified IAST as Mc
import LLVMEnv

import Data.Char
import Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as L
import Data.String
import Data.Word
import Control.Monad.State
import qualified Data.Map as Map

import LLVM.Pretty
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.AST.Name as LLVM
import qualified LLVM.AST.Constant as LLVM
import qualified LLVM.IRBuilder.Constant as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module as IR
import qualified LLVM.IRBuilder.Monad as IR

-- Construct the Haskell LLVM AST representation.

------------------------------------------------------------
-- LLVM AST Generation
------------------------------------------------------------

runGen :: String -> Mc.ITUnit -> LLVM.Module
runGen nm tunit = fst $ runState (IR.buildModuleT (fromString nm) (codeGen tunit)) env
  where
    env = Env { active = ST Map.empty, rest = []
              , externs = Map.empty
              , funcs = Map.empty
              , funcHasRet = False }

------------------------------------------------------------
-- Translation units
------------------------------------------------------------

codeGen :: Mc.ITUnit -> LLVMGen ()
codeGen (Mc.ITUnit tls) = do
  prnt <- IR.extern (LLVM.mkName "print") [LLVM.i32] LLVM.void
  addExtern prnt
  let x = \y -> case y of
                  (Mc.IGlobal decl) -> genGlobalDecl decl
                  (Mc.IFDef func)  -> genFunc func
  mapM_ x tls

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

genGlobalDecl :: (MonadState Env m, IR.MonadModuleBuilder m)
              => Mc.IDecl
              -> m ()
genGlobalDecl (Mc.IDecl CVoid id) = error "Can't have void decl with id"
genGlobalDecl (Mc.IDecl tpe id) = do
  d <- IR.global (LLVM.mkName id) (decideType tpe) (LLVM.Int (fromIntegral 32) 0)
  addToActive id d
  return ()

genDecl :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.IDecl
        -> m ()
genDecl (Mc.IDecl tpe id) = do
  d <- case tpe of
    CInt           -> IR.alloca LLVM.i32 Nothing 4
    CChar          -> IR.alloca LLVM.i8 Nothing 1
    CVoid          -> error "Can't have void decl with id"
    pn@(Pntr p)    -> IR.alloca (decideType pn) Nothing 8
    ar@(Array c t) -> IR.alloca (decideType ar) Nothing 8
  addToActive id d
  return ()

------------------------------------------------------------
-- Functions
------------------------------------------------------------

genFunc :: (MonadState Env m, IR.MonadModuleBuilder m, MonadFix m)
        => Mc.IFunc
        -> m ()
genFunc (Mc.IFunc tpe id params block) = mdo
  let ps = map mkParam params

  -- Insert function to env using mdo to allow
  -- recursive calls
  addFunc id f
  f <- IR.function (LLVM.mkName id) ps (decideType tpe) $ \ops -> do

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

    mapM_ f $ Mc.getIBlock block

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
        => Mc.IStmt
        -> m ()
genStmt (Mc.IBlockS bl) = do
  newActive
  let f = \y -> case y of
                  Left decl  -> genDecl decl
                  Right stmt -> genStmt stmt
  mapM_ f $ Mc.getIBlock bl
  dropActive
  return ()

-- Expr statements
genStmt (Mc.IExprS expr) = do
  genExpr expr
  return ()

-- If Else statements
genStmt (Mc.IIfElse expr stmt1 stmt2) = mdo
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
genStmt (Mc.IWhile expr stmt) = mdo
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
genStmt (Mc.IReturn x) = do
  modify $ \env -> env { funcHasRet = True }
  case x of
    Nothing -> do
                IR.retVoid
    Just x  -> do
                e <- genExpr x
                IR.ret e

-- Print statements
genStmt (Mc.IPrint expr) = do
  e <- genExpr expr
  env <- get
  let op = getPrint env
  IR.call op [(e, [])]
  return ()

-- Null statements
genStmt (Mc.INull) = return ()

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

-- Variables
genExpr :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.IExpr
        -> m LLVM.Operand
genExpr ((Mc.IVar id), tpe) = do
  env <- get
  let op = idFromEnv id env
  IR.load op 8

-- Array variables
genExpr ((Mc.IVarA id inx), tpe) = do
  env <- get
  let op = idFromEnv id env
  let indexes = map (IR.int32 . fromIntegral) inx
  p <- IR.gep op ([IR.int32 0] ++ indexes)
  IR.load p 8

-- Int Constants
genExpr ((Mc.IIConst int), _) = return $ constInt32 int

-- Char Constants
genExpr ((Mc.ICConst char), _) = return $ IR.int8 $ toInteger $ ord char

-- BinOps
genExpr ((Mc.IBinOp op e ee), tpe) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  case op of
    Add   -> IR.add e1 e2
    Subtr -> IR.sub e1 e2
    Mul   -> IR.mul e1 e2
    Div   -> IR.sdiv e1 e2
    Lt    -> IR.icmp LLVM.SLT e1 e2
    Gt    -> IR.icmp LLVM.SGT e1 e2
    Eq    -> IR.icmp LLVM.EQ e1 e2

-- Assignments
genExpr ((Mc.IAssign (Mc.IId id) expr), _) = do
  env <- get
  let var = idFromEnv id env
  e <- genExpr expr
  IR.store var 8 e
  return var

genExpr ((Mc.IAssign (Mc.IAId id inx) expr), _) = do
  env <- get
  let op = idFromEnv id env
  let indexes = map (IR.int32 . fromIntegral) inx
  e <- genExpr expr
  p <- IR.gep op ([IR.int32 0] ++ indexes)
  IR.store p 8 e
  return p

-- Function calls
genExpr ((Mc.IFCall id args), tpe) = do
  env <- get
  as <- mapM genExpr args
  let mkArgs = fmap (\y -> (y, [])) as
  IR.call (funcFromEnv id env) mkArgs

------------------------------------------------------------
-- Params
------------------------------------------------------------

genParam :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => (Mc.IParam, LLVM.Operand)
        -> m LLVM.Operand
genParam ((Mc.IParam _ id), (LLVM.LocalReference tpe nm)) = do
  env <- get
  let op = idFromEnv id env
  addr <- IR.alloca tpe Nothing 8
  IR.store addr 8 op
  return addr
genParam _ = error "Internal error: There should be only local refs in params"

