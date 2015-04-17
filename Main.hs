module Main where

eval :: String -> String
eval = id

main :: IO ()
main = do
    putStr "> "
    input <- getLine
    putStrLn $ eval input

    main
