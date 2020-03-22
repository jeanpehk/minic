{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module ConstrLLVM where

import Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as L

import Data.String

import qualified AST as Mc

import Checker
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

import LLVM.Pretty
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Name as LLVM
import qualified LLVM.AST.Constant as LLVM
import qualified LLVM.IRBuilder.Module as IR

-- This module constructs the llvm module

type Codegen a = Checker a

int32 :: LLVM.Type
int32 = LLVM.IntegerType 32

-- Change type from own AST into LLVM
decideType :: Mc.Type -> LLVM.Type
decideType Mc.CInt = LLVM.IntegerType 32

-- Changes all traversed blocks in env back to normal blocks
rebuildEnv :: Env -> Env
rebuildEnv env = Env { active = active env, blocks = used env, used = [] }

-- Simple test to try out running codegen
test :: IO ()
test = case fst (runCodegen ast env) of
        Left err -> T.putStrLn $ L.pack $ show err
        Right x  -> T.putStrLn $ ppllvm x
  where
    ast = Mc.TUnit [Mc.GDecl (Mc.Decl Mc.CInt "ok")]
    env = Env { active = ST (Map.fromList [("ok", Mc.CInt)])
              , blocks = []
              , used   = [] }

runCodegen :: Mc.TUnit -> Env -> (Either Error LLVM.Module, Env)
runCodegen tunit = (runState . runExceptT) (codeGen tunit)

codeGen :: Mc.TUnit -> Codegen LLVM.Module
codeGen (Mc.TUnit tls) = IR.buildModuleT "minic.ll" $ mdo
  let x = \y -> case y of
                  (Mc.GDecl decl) -> genDecl decl
                  (Mc.FDef func)  -> genFunc func
  mapM x tls

genDecl :: IR.MonadModuleBuilder m => Mc.Decl -> m LLVM.Operand
genDecl (Mc.Decl tpe id) = do
  IR.global (LLVM.Name (fromString id)) (decideType tpe) (LLVM.GlobalReference int32 (fromString id))

genFunc func = undefined

