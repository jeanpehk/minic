{-# LANGUAGE OverloadedStrings #-}

module Main where

import AST
import Checker
import ConstrLLVM
import qualified Data.Text.Lazy as L
import LLVM.Pretty
import Parser
import System.Environment
import System.Console.Haskeline
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)

-- Main starting point for minic
-- Uses a single file as input if such is given
-- Otherwise starts a repl
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fn] -> handleProg fn

-- Handle given program.
handleProg fn = do
  program <- readFile fn
  case parse tunit fn program of
    Left err  -> putStr (errorBundlePretty err)
    Right res -> do
                  pPrint res
                  case runChecker res of
                    (Left err, _)  -> putStrLn $ show err
                    (Right _, st) -> case fst (runCodegen res st) of
                                      Left err  -> putStrLn $ show err
                                      Right mod -> putStrLn $ L.unpack (ppllvm mod)

-- Starts a REPL
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
                              case fst (runCodegen res st) of
                                Left err  -> do {outputStrLn $ show err; loop}
                                Right mod -> do
                                               outputStrLn $ L.unpack (ppllvm mod)
                                               loop

