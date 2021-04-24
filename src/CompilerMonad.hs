module CompilerMonad where

import AST
import IAST
import Checker
import Parser

import Control.Monad.Except
import Control.Monad.State

import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

type CompilerMonad a = ExceptT String (StateT CompilerState IO) a

data CompilerState = CompilerState
  { fname :: Maybe FilePath
  , sourceFile :: Maybe String
  , ast :: Maybe TUnit
  , iAst :: Maybe ITUnit
  } deriving (Eq, Show)

parseC :: FilePath -> CompilerMonad TUnit
parseC fp = do
  src <- (lift . lift) (readFile fp)
  env <- get
  modify $ \cState -> cState { fname = Just fp, sourceFile = Just src }
  case parse tunit fp src of
    Left err -> throwError (errorBundlePretty err)
    Right rs -> return rs


checkC :: CompilerMonad ITUnit
checkC = do
  env <- get
  case ast env of
    Nothing  -> throwError "Cannot typecheck before parsing."
    Just ast -> do
      case runChecker ast of
        (Left err, _)    -> (throwError . show) err
        (Right chked, _) -> return chked

--    Left err  -> putStr (errorBundlePretty err)
--    Right res -> do
--                  --pPrint res
--                  case runChecker res of
--                    (Left err, _)  -> putStrLn $ show err
--                    (Right cres, st) -> do
--                                      let ast = runGen fn cres
--                                      --putStrLn $ L.unpack $ ppllvm $ ast
--                                      toLLVM "minic" ast

