module Main where

import System.IO

eval :: String -> String
eval = id

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
