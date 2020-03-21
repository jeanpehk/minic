module Main where

import Parser
import AST
import Checker
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)
import System.Environment
import System.Console.Haskeline

-- Main starting point for minic
-- Uses a single file as input if such is given
-- Otherwise starts a repl
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fn] -> printParser fn

-- Parses given input and prints the result
printParser fn = do
  program <- readFile fn
  case parse tunit fn program of
    Left  err -> putStr (errorBundlePretty err)
    Right res -> pPrint res

parseInp fn = case parse tunit "" fn of
                Left  err -> outputStrLn (errorBundlePretty err)
                Right res -> pPrint res

-- Starts a REPL
-- Prints out results of parsing user input
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
                          case fst (runChecker res) of
                            Left  err -> do {outputStrLn $ show err; loop}
                            Right res -> do
                              outputStrLn $ "Input typechecked without error."
                              loop
