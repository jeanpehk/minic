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

constInt32 :: Integer -> AST.Operand
constInt32 n = AST.ConstantOperand $ AST.Int (fromIntegral 32) n

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
-- Generation
------------------------------------------------------------

-- Translation units
codeGen :: Mc.TUnit -> Generator [AST.Operand]
codeGen (Mc.TUnit tls) = do
  let x = \y -> case y of
                  (Mc.GDecl decl) -> genDecl decl
                  (Mc.FDef func)  -> genFunc func
  mapM x tls

-- Top level declarations
genTl (Mc.GDecl (Mc.Decl Mc.CInt id)) = do
  IR.global (AST.mkName id) int32 (AST.Int (fromIntegral 32) 0)

-- Declarations
genDecl :: (MonadState Names m, IR.MonadModuleBuilder m)
        => Mc.Decl
        -> m AST.Operand
genDecl (Mc.Decl Mc.CInt id) = do
  st <- get
--  put $ Names { active = active st, rest = rest st}
  d <- IR.global (AST.mkName id) int32 (AST.Int (fromIntegral 32) 0)
  put $ Names { active = addToActive (active st) id d, rest = rest st}
  pure d

-- Functions
genFunc :: Mc.Func -> Generator AST.Operand
genFunc (Mc.Func tpe id params block) = mdo
  let ps = map mkParam params
  IR.function (AST.mkName id) ps (decideType tpe) $ \ops -> mdo

    -- Add params to symboltable
    st <- get
    put $ Names { active = paramsToST (active st) params ops, rest = rest st }
    allocs <- mapM allocParam [(p, o) | p <- params | o <- ops]
    nst <- get
    put $ Names { active = paramsToST (active st) params allocs, rest = rest st }

    let f = \y -> case y of
                    Left decl  -> genDecl decl
                    Right stmt -> genStmt stmt
    bls <- mapM f $ Mc.getBlock block

    IR.ret $ IR.int32 0

-- Generate statements

-- Block statements
genStmt :: (MonadState Names m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.Stmt
        -> m AST.Operand
genStmt (Mc.BlockStmt bl) = undefined

-- Expr statements
genStmt (Mc.ExprStmt expr) = genExpr expr

-- Generate expressions
genExpr :: (MonadState Names m, IR.MonadModuleBuilder m, IR.MonadIRBuilder m)
        => Mc.Expr
        -> m AST.Operand
genExpr (Mc.Var id) = do
  st <- get
  let op = idFromNames id st
  IR.load op 8

genExpr (Mc.Add e ee) = do
  e1 <- genExpr e
  e2 <- genExpr ee
  IR.add e1 e2

-- Allocate params
allocParam ((Mc.Param _ id), (AST.LocalReference tpe nm)) = do
  st <- get
  let op = idFromNames id st
  IR.alloca tpe (Just op) 32
allocParams _ _ = error "There should be only local refs in params"

