module Scheme.REPL where

import Data.List
import System.IO

import Scheme.Parser
import Scheme.Evaluator
import Scheme.Printer

lispRepl :: Bool -> IO ()
lispRepl verbose = do
  putStr "> "
  hFlush stdout

  line <- getLine
  let ast = parseLisp line

  let evaluated = evalSource ast

  if verbose
    then print evaluated
    else putStrLn $ intercalate "\n" $ map printLisp $ evaluationResult evaluated

  hFlush stdout

  lispRepl verbose
