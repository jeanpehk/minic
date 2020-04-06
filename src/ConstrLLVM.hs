{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module ConstrLLVM where

import Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as L

import Data.String
import Data.Word

import qualified AST as Mc

import Control.Monad.State
import qualified Data.Map as Map

import LLVM.Pretty
import qualified LLVM.AST as AST
import qualified LLVM.AST.IntegerPredicate as AST
import qualified LLVM.AST.Name as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.IRBuilder.Constant as IR
import qualified LLVM.IRBuilder.Instruction as IR
import qualified LLVM.IRBuilder.Module as IR
import qualified LLVM.IRBuilder.Monad as IR

-- This module constructs the Haskell LLVM AST representation.

------------------------------------------------------------
-- Datatypes
------------------------------------------------------------

type Generator = IR.ModuleBuilderT (State Names)

data Names = Names { active :: ST, rest :: [ST] }
  deriving (Eq, Show)

newtype ST = ST { getST :: Map.Map String AST.Operand }
  deriving (Eq, Show)

------------------------------------------------------------
-- Functions for building an LLVM Module
------------------------------------------------------------

int32 :: AST.Type
int32 = AST.IntegerType 32

constInt32 :: Int -> AST.Operand
constInt32 n = AST.ConstantOperand $ AST.Int (fromIntegral 32) (fromIntegral n)

-- Change type from own AST into LLVM
decideType :: Mc.Type -> AST.Type
decideType Mc.CInt = int32

-- Starting point for generating an LLVM module
runGen :: String -> Mc.TUnit -> (AST.Module, Names)
runGen nm tunit = runState (IR.buildModuleT (fromString nm) (codeGen tunit)) ns
  where
    ns = Names { active = ST Map.empty, rest = [] }

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

-- Turns a minic param into llvm param
mkParam :: Mc.Param -> (AST.Type, IR.ParameterName)
mkParam (Mc.Param tpe id) = (decideType tpe, IR.ParameterName (fromString id))

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

-- New active for Names to avoid name collisions
-- when moving into a new block in the source language.
newActive :: Names -> Names
newActive ns = Names { active = ST Map.empty, rest = active ns:rest ns }

-- Drop active ST from Names when moving out of a block
-- in the source language.
dropActive :: Names -> Names
dropActive ns = case rest ns of
                  []     -> Names { active = ST Map.empty, rest = [] }
                  (x:xs) -> Names { active = x, rest = xs }

idFromNames :: String -> Names -> AST.Operand
idFromNames id ns = case Map.lookup id (getST (active ns)) of
                      Nothing -> lookUpRest (rest ns)
                      Just x  -> x
  where
    lookUpRest (r:rs) = case Map.lookup id (getST r) of
                          Nothing -> lookUpRest rs
                          Just x  -> x
    lookUpRest [] = error $ "Var: " ++ id ++ " not found in listed Names"

getName :: AST.Operand -> AST.Name
getName op = case op of
               AST.LocalReference tpe nm -> nm
               _                         -> error "Only local refs supported"

------------------------------------------------------------
-- LLVM AST Generation
------------------------------------------------------------

------------------------------------------------------------
-- Translation units
------------------------------------------------------------

codeGen :: Mc.TUnit -> Generator [AST.Operand]
codeGen (Mc.TUnit tls) = do
  let x = \y -> case y of
                  (Mc.GDecl decl) -> genGlobalDecl decl
                  (Mc.FDef func)  -> genFunc func
  mapM x tls

------------------------------------------------------------
-- Top level declarations
------------------------------------------------------------

genTl (Mc.GDecl (Mc.Decl Mc.CInt id)) = do
  IR.global (AST.mkName id) int32 (AST.Int (fromIntegral 32) 0)

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

genGlobalDecl :: (MonadState Names m, IR.MonadModuleBuilder m)
        => Mc.Decl
        -> m AST.Operand
genGlobalDecl (Mc.Decl Mc.CInt id) = do
  st <- get
  d <- IR.global (AST.mkName id) int32 (AST.Int (fromIntegral 32) 0)
  put $ Names { active = addToActive (active st) id d, rest = rest st}
  return d

genDecl :: (MonadState Names m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.Decl
        -> m AST.Operand
genDecl (Mc.Decl Mc.CInt id) = do
  st <- get
  d <- IR.alloca int32 Nothing 8
  put $ Names { active = addToActive (active st) id d, rest = rest st}
  return d


------------------------------------------------------------
-- Functions
------------------------------------------------------------

genFunc :: Mc.Func -> Generator AST.Operand
genFunc (Mc.Func tpe id params block) = mdo
  let ps = map mkParam params
  IR.function (AST.mkName id) ps (decideType tpe) $ \ops -> mdo

    -- Add params to symboltable
    ns <- get
    let nns = newActive ns
    put $ Names { active = paramsToST (active nns) params ops, rest = rest nns }
    gparams <- mapM genParam [(p, o) | p <- params | o <- ops]
    nsWithParams <- get
    put $ Names { active = paramsToST (active nsWithParams) params gparams
                , rest = rest nsWithParams }

    let f = \y -> case y of
                    Left decl  -> genDecl decl
                    Right stmt -> genStmt stmt
    mapM f $ Mc.getBlock block

    IR.ret $ IR.int32 0

------------------------------------------------------------
-- Statements
------------------------------------------------------------

-- Block statements
genStmt :: (MonadState Names m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.Stmt
        -> m AST.Operand
genStmt (Mc.BlockStmt bl) = do
  ns <- get
  let nns = newActive ns
  put $ Names { active = active nns, rest = rest nns }
  let f = \y -> case y of
                  Left decl  -> genDecl decl
                  Right stmt -> genStmt stmt
  mapM f $ Mc.getBlock bl
  return $ IR.int32 0

-- Expr statements
genStmt (Mc.ExprStmt expr) = genExpr expr

-- Null statements
genStmt (Mc.Null) = return $ IR.int32 0

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

-- Variables
genExpr :: (MonadState Names m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.Expr
        -> m AST.Operand
genExpr (Mc.Var id) = do
  st <- get
  let op = idFromNames id st
  IR.load op 8

-- Int Constant
genExpr (Mc.IntConst int) = return $ constInt32 int

-- Addition
genExpr (Mc.Add e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.add e1 e2

-- Subtraction
genExpr (Mc.Subtr e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.sub e1 e2

-- Multiplication
genExpr (Mc.Mul e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.mul e1 e2

-- Division
genExpr (Mc.Div e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.sdiv e1 e2

-- LT
genExpr (Mc.Lt e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.icmp AST.SLT e1 e2

-- GT
genExpr (Mc.Gt e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.icmp AST.SGT e1 e2

-- EQ
genExpr (Mc.Eq e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.icmp AST.EQ e1 e2

-- Assignment
genExpr (Mc.Assign id expr) = do
  ns <- get
  let var = idFromNames id ns
  e <- genExpr expr
  IR.store e 8 var
  return var

------------------------------------------------------------
-- Params
------------------------------------------------------------

genParam ((Mc.Param _ id), (AST.LocalReference tpe nm)) = do
  st <- get
  let op = idFromNames id st
  addr <- IR.alloca tpe (Just op) 8
  IR.store op 8 addr
  return addr
genParam _ = error "There should be only local refs in params"

