{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import LLVM.IRBuilder.Monad

-- This module constructs the Haskell LLVM AST representation.

------------------------------------------------------------
-- Datatypes
------------------------------------------------------------

type Generator a = IR.ModuleBuilderT (State Names) a

newtype Names = Names { getNames :: [Map.Map String Int] }
  deriving (Eq, Show)

------------------------------------------------------------
-- Functions for building an LLVM Module
------------------------------------------------------------

int32 :: AST.Type
int32 = AST.IntegerType 32

-- Change type from own AST into LLVM
decideType :: Mc.Type -> AST.Type
decideType Mc.CInt = int32

runGen tunit = runState (IR.buildModuleT "ok" (codeGen tunit)) ns
  where
    ns = Names [Map.empty]

test :: IO ()
test = do
  let res = snd $ runGen (Mc.TUnit [(Mc.GDecl (Mc.Decl Mc.CInt "hello"))
                            , (Mc.FDef (Mc.Func Mc.CInt "func"
                                       [Mc.Param Mc.CInt "ok"] (Mc.Block [])))])
  Prelude.putStrLn $ show res

------------------------------------------------------------
-- Generation
------------------------------------------------------------

-- Translation units
codeGen :: Mc.TUnit -> Generator [AST.Operand]
codeGen (Mc.TUnit tls) = do
  let x = \y -> case y of
                  (Mc.GDecl decl) -> genDecl decl
                  (Mc.FDef func)  -> genFunc func
  ns <- get
  put $ Names $ (Map.insert "maintypestuff" 0 Map.empty):(getNames ns)
  mapM x tls

-- Top level declarations
genTl (Mc.GDecl (Mc.Decl Mc.CInt id)) = do
  IR.global (AST.mkName id) int32 (AST.Int (fromIntegral 32) 0)

-- Declarations
genDecl :: Mc.Decl -> Generator AST.Operand
genDecl (Mc.Decl Mc.CInt id) = do
  ns <- get
  put $ Names $ (Map.insert "DECLSDECLSDECLs" 0 Map.empty):(getNames ns)
  IR.global (AST.mkName id) int32 (AST.Int (fromIntegral 32) 0)

-- Functions
genFunc :: Mc.Func -> Generator AST.Operand
genFunc (Mc.Func tpe id params block) = mdo
  let ps = map genParam params
  ns <- get
  put $ Names $ (Map.insert "FUNCITUP" 0 Map.empty):(getNames ns)
  IR.function (AST.mkName id) ps (decideType tpe) $ \ops -> mdo
      IR.alloca int32 Nothing 32
      IR.ret (head ops)

-- Parameters
genParam :: Mc.Param -> (AST.Type, IR.ParameterName)
genParam (Mc.Param tpe id) = (decideType tpe, IR.ParameterName (fromString id))

