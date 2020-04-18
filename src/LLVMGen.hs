{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module LLVMGen where

import Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as L

import Data.Char
import Data.String
import Data.Word

import qualified AST as Mc

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
-- Datatypes
------------------------------------------------------------

type Generator = IR.ModuleBuilderT (State Env)

data Env = Env { active :: ST, rest :: [ST]
               , funcs :: Map.Map String AST.Operand
               , funcHasRet :: Bool }
  deriving (Eq, Show)

newtype ST = ST { getST :: Map.Map String AST.Operand }
  deriving (Eq, Show)


-- Simple test function
test :: IO ()
test = do
  let res = snd $
            runGen "test" (Mc.TUnit [(Mc.GDecl (Mc.Decl Mc.CInt "hello"))
                          , (Mc.FDef (Mc.Func Mc.CInt "func"
                            [Mc.Param Mc.CInt "ok", Mc.Param Mc.CInt "abc"]
                              (Mc.Block
                                [Left (Mc.Decl Mc.CInt "abc3"),
                                 Right (Mc.ExprStmt (Mc.Var "abci"))])))])
  Prelude.putStrLn $ show res

------------------------------------------------------------
-- Helper functions for building an LLVM Module
------------------------------------------------------------

constInt32 :: Int -> AST.Operand
constInt32 n = AST.ConstantOperand $ AST.Int (fromIntegral 32) (fromIntegral n)

-- Change type from own AST into LLVM
decideType :: Mc.Type -> AST.Type
decideType Mc.CInt   = AST.i32
decideType Mc.CChar  = AST.i8
decideType Mc.CVoid  = AST.void
decideType (Mc.Pntr p) = AST.ptr (decideType p)

-- Turns a minic param into llvm param
mkParam :: Mc.Param -> (AST.Type, IR.ParameterName)
mkParam (Mc.Param Mc.CInt id)     = (AST.i32, IR.ParameterName (fromString id))
mkParam (Mc.Param Mc.CChar id)    = (AST.i8, IR.ParameterName (fromString id))
mkParam (Mc.Param (Mc.Pntr p) id) =
  (decideType (Mc.Pntr p), IR.ParameterName (fromString id))
mkParam (Mc.ParamNoId _)          = error "TODO"
mkParam (Mc.Param Mc.CVoid _)     = error "Can't have param with void type and id"

-- Add params to Symbol Table
paramsToST :: ST -> [Mc.Param] -> [AST.Operand] -> ST
paramsToST st (x:xs) (y:ys) = let insert i o = ST $ Map.insert (Mc.getPID i) o (getST st)
                              in paramsToST (insert x y) xs ys
paramsToST st [] _ = st
paramsToST st _ [] = st

-- Add new var to active Symbol Table
addToActive :: ST -> String -> AST.Operand -> ST
addToActive st id op = case Map.lookup id (getST st) of
                           Nothing -> ST $ Map.insert id op (getST st)
                           Just x  -> error $ "Id: " ++ id ++ " already declared"

-- New active for Env to avoid name collisions
-- when moving into a new block in the source language.
newActive :: Env -> Env
newActive env = Env { active = ST Map.empty, rest = active env:rest env
                    , funcs = funcs env
                    , funcHasRet = funcHasRet env }

-- Drop active ST from Env when moving out of a block
-- in the source language.
dropActive :: Env -> Env
dropActive env = case rest env of
                  []     -> Env { active = ST Map.empty, rest = []
                                , funcs = funcs env
                                , funcHasRet = funcHasRet env }
                  (x:xs) -> Env { active = x, rest = xs
                                , funcs = funcs env
                                , funcHasRet = funcHasRet env }

-- Get id from Environment, excluding functions
idFromEnv :: String -> Env -> AST.Operand
idFromEnv id env = case Map.lookup id (getST (active env)) of
                      Nothing -> lookUpRest (rest env)
                      Just x  -> x
  where
    lookUpRest (r:rs) = case Map.lookup id (getST r) of
                          Nothing -> lookUpRest rs
                          Just x  -> x
    lookUpRest [] = error $ "Var: " ++ id ++ " not found in listed Env"

-- Get function from Env
funcFromEnv :: String -> Env -> AST.Operand
funcFromEnv id env = case Map.lookup id (funcs env) of
                      Nothing -> error $ "Function: '" ++ id ++ "' not found"
                      Just x  -> x

getName :: AST.Operand -> AST.Name
getName op = case op of
               AST.LocalReference tpe nm -> nm
               _                         -> error "Only local refs supported"

------------------------------------------------------------
-- LLVM AST Generation
------------------------------------------------------------

runGen :: String -> Mc.TUnit -> (AST.Module, Env)
runGen nm tunit = runState (IR.buildModuleT (fromString nm) (codeGen tunit)) env
  where
    env = Env { active = ST Map.empty, rest = []
              , funcs = Map.empty
              , funcHasRet = False }

------------------------------------------------------------
-- Translation units
------------------------------------------------------------

codeGen :: Mc.TUnit -> Generator ()
codeGen (Mc.TUnit tls) = do
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
  put $ Env { active = addToActive (active env) id d, rest = rest env
            , funcs = funcs env
            , funcHasRet = funcHasRet env}
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
  put $ Env { active = addToActive (active env) id d, rest = rest env
            , funcs = funcs env
            , funcHasRet = funcHasRet env }
  return ()

------------------------------------------------------------
-- Functions
------------------------------------------------------------

genFunc (Mc.Func tpe id params block) = mdo
  let ps = map mkParam params
  env <- get
  -- Insert function to env using mdo to allow
  -- recursive calls
  put $ Env { active = active env, rest = rest env
            , funcs = Map.insert id f (funcs env)
            , funcHasRet = funcHasRet env }

  f <- IR.function (AST.mkName id) ps (decideType tpe) $ \ops -> do

    -- Add params to symboltable
    env <- get
    let envv = newActive env
    put $ Env { active = paramsToST (active envv) params ops, rest = rest envv
              , funcs = funcs envv
              , funcHasRet = funcHasRet envv }
    -- generate params
    gparams <- mapM genParam [(p, o) | p <- params | o <- ops]
    nsWithParams <- get
    -- add params to env
    put $ Env { active = paramsToST (active nsWithParams) params gparams
              , rest = rest nsWithParams
              , funcs = funcs nsWithParams
              , funcHasRet = funcHasRet nsWithParams }

    let ff = \y -> case y of
                    Left decl  -> genDecl decl
                    Right stmt -> genStmt stmt

    mapM_ ff $ Mc.getBlock block

    -- Check whether return stmt is already generated
    envAfter <- get
    case funcHasRet envAfter of
      False -> do
                  IR.ret $ IR.int32 0
                  return ()
      True  -> do
                  put $ Env { active = active envAfter
                            , rest = rest envAfter
                            , funcs = funcs env
                            , funcHasRet = False }
                  return ()

  return ()

------------------------------------------------------------
-- Statements
------------------------------------------------------------

-- Block statements
genStmt :: (MonadState Env m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m, MonadFix m)
        => Mc.Stmt
        -> m ()
genStmt (Mc.BlockStmt bl) = do
  env <- get
  let envv = newActive env
  put $ Env { active = active envv, rest = rest envv
            , funcs = funcs env
            , funcHasRet = funcHasRet envv }
  let f = \y -> case y of
                  Left decl  -> genDecl decl
                  Right stmt -> genStmt stmt
  mapM_ f $ Mc.getBlock bl
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
  env <- get
  put $ Env { active = active env, rest = rest env
            , funcs = funcs env
            , funcHasRet = True }
  case x of
    Nothing -> do
                IR.retVoid
    Just x  -> do
                e <- genExpr x
                IR.ret e

-- Print statements
genStmt (Mc.Print expr) = do
  e <- genExpr expr
  prnt <- IR.extern (AST.mkName "print") [AST.i32] AST.void
  IR.call prnt [(e, [])]
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
genExpr (Mc.FCall id) = do
  env <- get
  IR.call (funcFromEnv id env) []

------------------------------------------------------------
-- Params
------------------------------------------------------------

genParam ((Mc.Param _ id), (AST.LocalReference tpe nm)) = do
  env <- get
  let op = idFromEnv id env
  addr <- IR.alloca tpe Nothing 8
  IR.store addr 8 op
  return addr
genParam _ = error "There should be only local refs in params"

