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
    [fn] -> handleProg fn

-- Handle given program.
-- ATM runs the parses and prints it's result on the screen,
-- if the result was correct proceeds with typechecking, which
-- also prints it's result on the screen.
handleProg fn = do
  program <- readFile fn
  case parse tunit fn program of
    Left err  -> putStr (errorBundlePretty err)
    Right res -> do
                  pPrint res
                  case fst (runChecker res) of
                    Left err  -> putStrLn $ show err
                    Right res -> putStrLn $ "Program typechecker found no errors."

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
                          case runChecker res of
                            (Left  err, _) -> do {outputStrLn $ show err; loop}
                            (Right res, st) -> do
                              outputStrLn $ "Input typechecked without error."
                              loop
