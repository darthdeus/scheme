module Scheme.REPL where

import Data.List
import System.IO

import Scheme.Types
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Printer

lispRepl :: Env -> Bool -> IO ()
lispRepl env verbose = do
  putStr "> "
  hFlush stdout

  line <- getLine
  let ast = parseLisp line

  let evaluated@(Evaluated _ newEnv) = evalSource env ast

  if verbose
    then print evaluated
    else putStrLn $ intercalate "\n" $ map printLisp $ evaluationResult evaluated

  hFlush stdout

  lispRepl newEnv verbose
