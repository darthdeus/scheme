module Main where

import System.IO

eval :: String -> String
eval = id

main :: IO ()
main = do
    putStr "> "
    input <- getLine
    putStrLn $ eval input

    main
