module Checker where

import AST
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- This module contains type and context checking for minic.

data SymbolTable = ST { getSt :: Map.Map Id Type }

-- Type environment for the program
data Env = Env { active :: SymbolTable, blocks :: [SymbolTable] }

-- Error datatypes
data Error
  = TError Id Type           -- Type errors
  | DError Id                -- Declaration errors
  | SError String            -- Syntax errors
  | CError String            -- Compiler errors
  deriving (Eq, Show)

-- Datatype for the checker
-- Except for error handling and
-- State to contain the environment
type Checker a = ExceptT Error (State Env) a

-- Run the checker
runChecker :: TUnit -> (Either Error (), Env)
runChecker tunit = (runState . runExceptT) (checkTu tunit) env
  where
    env = Env { active = ST Map.empty, blocks = [ST Map.empty] }

-- Check translation unit
checkTu :: TUnit -> Checker ()
checkTu (TUnit tl) = do
  mapM checkTl tl
  return ()

-- Add a new identifier into the environment.
addId :: Type -> Id -> Env -> Env
addId tpe id env = Env { active = ST (Map.insert id tpe (getSt (active env)))
                       , blocks = blocks env }

-- Check top-level
checkTl :: TL -> Checker ()
checkTl (GDecl (Decl CVoid _ )) = throwError $ SError "Void cannot have an identifier"
checkTl (GDecl (Decl tpe id))   = do
  env <- get
  case Map.lookup id (getSt (active env)) of
    Nothing -> put $ addId tpe id env
    Just x  -> throwError $ DError id

checkTl (FDef func) = undefined

