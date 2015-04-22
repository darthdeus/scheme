module Main where

import System.IO
import Scheme.Parser
import Scheme.Evaluator

repl :: IO ()
repl = do
    putStr "> "
    hFlush stdout
    input <- getLine
    putStrLn $ eval input

    main

main :: IO ()
main = do
    putStrLn ";; Scheme REPL"
    repl
