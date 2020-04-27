{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST
import Checker
import LLVMGen

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as L
import Parser
import System.Environment
import System.Console.Haskeline

import LLVM.Analysis
import qualified LLVM.AST as AST
import LLVM.Context
import qualified LLVM.Exception as Exception
import LLVM.Module
import LLVM.Target
import qualified LLVM.Relocation as Relocation
import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel

import LLVM.Pretty

import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

-- Main starting point for minic
-- Uses a single file as input if such is given
-- Otherwise starts a repl
main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl
    (x:xs) -> compileProg x

-- compile given program.
compileProg :: FilePath -> IO ()
compileProg fn = do
  program <- readFile fn
  case parse tunit fn program of
    Left err  -> putStr (errorBundlePretty err)
    Right res -> do
                  --pPrint res
                  case runChecker res of
                    (Left err, _)  -> putStrLn $ show err
                    (Right _, st) -> do
                                      let ast = runGen fn res
                                      --putStrLn $ L.unpack $ ppllvm $ ast
                                      toLLVM "minic" ast

-- Starts a REPL that prints out inputs in LLVM text format
repl :: IO ()
repl = do
  putStrLn "minic"
  putStrLn "Type 'q' to quit."
  runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "minic> "
      case minput of
        Nothing -> return ()
        Just "q" -> return ()
        Just input -> do
          case parse tunit "" input of
            Left  err -> do {outputStrLn (errorBundlePretty err); loop}
            Right res -> do
                          outputStrLn $ show res
                          case runChecker res of
                            (Left  err, _) -> do {outputStrLn $ show err; loop}
                            (Right _, st) -> do
                              let ast = runGen "repl" res
                              outputStrLn $ L.unpack $ ppllvm ast
                              loop


-- Transfer pure hs module to LLVM
toLLVM :: FilePath -> AST.Module -> IO ()
toLLVM fp mod =
  withContext $ \ctx ->
    withModuleFromAST ctx mod $ \llvmMod ->
      (do
        verify llvmMod
        withHostTargetMachine Relocation.Default CodeModel.Default CodeGenOpt.Default
          $ \tgt ->
            writeObjectToFile tgt (File fp) llvmMod)
      `catch` (\err@(Exception.VerifyException e) ->
        putStrLn "Exception\n" >> putStrLn e)

