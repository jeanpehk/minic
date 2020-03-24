{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module ConstrLLVM where

import Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as L

import Data.String
import Data.Word

import qualified AST as Mc

import Checker
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import LLVM.Pretty
import qualified LLVM.AST as IR
import qualified LLVM.AST.Name as IR
import qualified LLVM.AST.Constant as IR
import qualified LLVM.IRBuilder.Module as IR

-- This module constructs the llvm module

------------------------------------------------------------
-- Datatypes
------------------------------------------------------------

type Codegen a = Checker a

type GenState a = (State Gen a)

data Gen
  = Gen { irblocks :: [Map.Map Block IR.Name]
        , env      :: [ST] }
  deriving (Eq, Show)

data Block
  = Block { idx :: Int
          , stack :: [IR.Named IR.Instruction] }
  deriving (Eq, Show)

newtype ST = ST (Map.Map String IR.Operand)
  deriving (Eq, Show)

------------------------------------------------------------
-- Functions for building an LLVM Module
------------------------------------------------------------

int32 :: IR.Type
int32 = IR.IntegerType 32

-- Change type from own AST into LLVM
decideType :: Mc.Type -> IR.Type
decideType Mc.CInt = int32

--runCodegen :: Mc.TUnit -> Env -> (Either Error IR.Module, Env)
--runCodegen tunit = (runState . runExceptT) (codeGen tunit)

runGen :: Mc.TUnit -> (IR.Module, Gen)
runGen tunit = runState (codeGen tunit) gen
  where
    gen = Gen { irblocks = [], env = [] }


codeGen :: Mc.TUnit -> GenState IR.Module
codeGen (Mc.TUnit tls) = IR.buildModuleT "minic.ll" $ mdo
  let x = \y -> case y of
                  (Mc.GDecl decl) -> genDecl decl
                  (Mc.FDef func)  -> genFunc func
  mapM x tls

genDecl :: IR.MonadModuleBuilder m => Mc.Decl -> m IR.Operand
genDecl (Mc.Decl (Mc.CInt) id) = 
  IR.global (IR.mkName id) int32 (IR.Int (fromIntegral 32) 0)

genFunc :: IR.MonadModuleBuilder m => Mc.Func -> m IR.Operand
genFunc (Mc.Func tpe id params block) = undefined

