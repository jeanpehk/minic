module Main where

import Parser
import AST
import Text.Megaparsec
import Text.Pretty.Simple (pPrint)
import System.Environment

main :: IO ()
main = do
  [arg] <- getArgs
  printParser arg

printParser fn = do
  program <- readFile fn
  case parse tunit fn program of
    Left err  -> putStr (errorBundlePretty err)
    Right res -> pPrint res

