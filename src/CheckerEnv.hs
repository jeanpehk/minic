module CheckerEnv where

import AST
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map

-- Env for type and context checker

------------------------------------------------------------
-- Datatypes
------------------------------------------------------------

type Checker a = ExceptT Error (State Env) a

-- Error datatypes
data Error
  = TError String            -- General type errors
  | SError String            -- Syntax errors
  deriving (Eq, Show)

data SymbolTable = ST { getSt :: Map.Map Id Type }
  deriving (Eq, Show)

data Env = Env { active :: SymbolTable
               , blocks :: [SymbolTable]
               , funcs  :: Map.Map Id (Type, [Param])
               , used   :: [SymbolTable] }
  deriving (Eq, Show)

-- Error text for declaration errors
dError :: Id -> String
dError id = "id: '" ++ id ++ "' already declared"

------------------------------------------------------------
-- Env manipulation functions
------------------------------------------------------------

-- Adds a new block into Env and makes it active
addBlock :: Checker ()
addBlock = modify $ \env -> env { active = ST Map.empty
                                , blocks = active env:blocks env }

-- Drops the current active block active ones
dropBlock :: Checker ()
dropBlock =
  modify $ \env -> case blocks env of
                    (x:xs) -> env { active = x, blocks = xs
                                  , used   = active env:used env }
                    []     -> env { active = ST Map.empty, blocks = [] }

-- Look for id from blocks incase it is not in the active one
lookFromBlocks :: Id -> [SymbolTable] -> Maybe Type
lookFromBlocks id (x:xs) = case Map.lookup id (getSt x) of
                             Nothing -> lookFromBlocks id xs
                             Just x  -> Just x
lookFromBlocks id []  = Nothing

-- Look for id from active symbol table
-- Used for declaring values
getId :: Id -> Env -> Maybe Type
getId id env = Map.lookup id (getSt (active env))

-- Get types of already declared variables from the entire Env
getDeclaredId :: Id -> Env -> Maybe Type
getDeclaredId id env = case Map.lookup id st of
                 Nothing -> case lookFromBlocks id (blocks env) of
                              Nothing -> do
                                          r <- Map.lookup id (funcs env)
                                          return $ fst r
                              Just x  -> Just x
                 Just x  -> Just x
  where
    st = getSt (active env)

-- Look for id from functions
-- Used for declaring functions
getFunc :: Id -> Env -> Maybe Type
getFunc id env = case Map.lookup id (funcs env) of
                   Nothing -> Map.lookup id $ getSt (active env)
                   Just x  -> Just $ fst x

-- Add a new identifier into the environment
addId :: Type -> Id -> Checker ()
addId tpe id = modify $ \env ->
  env { active = ST (Map.insert id tpe (getSt (active env))) }

addFunc :: Type -> [Param] -> Id -> Checker ()
addFunc tpe ps id = modify $ \env ->
  env { funcs = Map.insert id (tpe, ps) (funcs env) }

------------------------------------------------------------
-- Type comparisons for expressions
------------------------------------------------------------

compareTypes :: Type -> Type -> Either Error Type
compareTypes (CInt) (CInt) = Right CInt
compareTypes (CChar) (CChar) = Right CInt
compareTypes (CChar) (CInt) = Right CInt
compareTypes (CInt) (CChar) = Right CInt
compareTypes (Pntr a) (Pntr b) =
  case compareTypes a b of
    Left err -> Left err
    Right t  -> Right $ Pntr t
compareTypes _ (Pntr _) = Left $ TError "Cannot combine ptr with non ptr"
compareTypes (Pntr _) _ = Left $ TError "Cannot combine ptr with non ptr"
compareTypes (CVoid) _ = Left $ TError "Cannot combine Void with another type"
compareTypes _ (CVoid) = Left $ TError "Cannot combine Void with another type"

