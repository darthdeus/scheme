module Main where

import Data.List
import System.Environment
import System.Directory

import Scheme.Printer
import Scheme.Parser
import Scheme.Evaluator
import Scheme.REPL

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["-i"] -> lispRepl emptyEnv False
    ["-i", "-v"] -> lispRepl emptyEnv True

    (file:rest) -> do
      ok <- doesFileExist file

      let verbose =
            case rest of
              ["-v"] -> True
              _ -> False

      if ok
        then interpretFile file verbose
        else putStrLn $ "File " ++ file ++ " doesn't exist."

    _ -> putStrLn "Usage: lisp ( file [-v] | -i [-v] )\n\n\t-i\tStarts the REPL.\n\t-v\tDisplays evaluation details.\n"

interpretFile :: FilePath -> Bool -> IO ()
interpretFile path verbose = do
  content <- readFile path

  let evaluated = evalSource emptyEnv $ parseLisp content

  if verbose
    then print evaluated
    else putStrLn $ intercalate "\n" $ map printLisp $ evaluationResult evaluated
