{-# LANGUAGE FlexibleContexts #-}

module LLVMEnv where

import qualified AST as Mc

import Data.String
import Control.Monad.State
import qualified Data.Map as Map

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Type as AST
import qualified LLVM.IRBuilder.Module as IR
import qualified LLVM.IRBuilder.Monad as IR

------------------------------------------------------------
-- Datatypes
------------------------------------------------------------

type LLVMGen = IR.ModuleBuilderT (State Env)

data Env = Env { active :: ST, rest :: [ST]
               , externs :: Map.Map String AST.Operand
               , funcs :: Map.Map String AST.Operand
               , funcHasRet :: Bool }
  deriving (Eq, Show)

newtype ST = ST { getST :: Map.Map String AST.Operand }
  deriving (Eq, Show)

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
decideType (Mc.Array c t) = AST.ArrayType (fromIntegral c) (decideType t)

-- Turns a minic param into llvm param
mkParam :: Mc.Param -> (AST.Type, IR.ParameterName)
mkParam (Mc.Param Mc.CInt id)     = (AST.i32, IR.ParameterName (fromString id))
mkParam (Mc.Param Mc.CChar id)    = (AST.i8, IR.ParameterName (fromString id))
mkParam (Mc.Param pntr@(Mc.Pntr p) id) =
  (decideType pntr, IR.ParameterName (fromString id))
mkParam (Mc.Param arr@(Mc.Array c t) id) = (decideType arr, fromString id)
mkParam (Mc.ParamNoId _)          = error "TODO"
mkParam (Mc.Param Mc.CVoid _)     = error "Can't have param with void type and id"

-- Add params to Env
paramsToEnv :: MonadState Env m => [Mc.Param] -> [AST.Operand] -> m ()
paramsToEnv ps ops = modify $ \env ->
  env { active = paramsToST (active env) ps ops }

-- Add params to Symbol Table
paramsToST :: ST -> [Mc.Param] -> [AST.Operand] -> ST
paramsToST st (x:xs) (y:ys) = let insert i o = ST $ Map.insert (Mc.getPID i) o (getST st)
                              in paramsToST (insert x y) xs ys
paramsToST st [] _ = st
paramsToST st _ [] = st

-- Add new var to active Symbol Table
addToActive :: MonadState Env m => String -> AST.Operand -> m ()
addToActive id op = modify $ \env ->
  case Map.lookup id (getST (active env)) of
    Nothing -> env { active = ST (Map.insert id op (getST (active env))) }
    Just x  -> error $ "Internal error: id " ++ id ++ " already declared"

-- Add new function to env
addFunc :: MonadState Env m => String -> AST.Operand -> m ()
addFunc id op = modify $ \env ->
  case Map.lookup id (funcs env) of
    Nothing -> env { funcs = Map.insert id op (funcs env) }
    Just _  -> error $ "Internal error: function " ++ id ++ " already declared"

-- Add new external function to env
addExtern :: MonadState Env m => AST.Operand -> m ()
addExtern op = modify $ \env ->
  env { externs = Map.insert "print" op (externs env) }

-- Get external print function from env,
-- should always exist since there is no currently no import functionality
-- and it is always included.
getPrint :: Env -> AST.Operand
getPrint env = case Map.lookup "print" (externs env) of
                Nothing -> error "Internal error: \
                                 \Extern function print should always be in env"
                Just op -> op

-- New active for Env to avoid name collisions
-- when moving into a new block in the source language.
newActive :: MonadState Env m => m ()
newActive = modify $ \env ->
  env { active = ST Map.empty, rest = active env:rest env }

-- Drop active ST from Env when moving out of a block
-- in the source language.
dropActive :: MonadState Env m => m ()
dropActive = modify $ \env ->
  case rest env of
    []     -> env { active = ST Map.empty }
    (x:xs) -> env { active = x, rest = xs }

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

