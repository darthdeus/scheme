module Scheme.Types where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.State

type Env = [(String, AST)]

-- (define factorial
--           (lambda (n)
--              (if (= n 0)
--                1
--                (* n (factorial (- n 1)))))
--
-- (factorial 5)
-- ((lambda (x) (+ 1 x)) 3)

numberOp :: (Int -> Int -> AST) -> AST -> AST -> State Env AST
numberOp f x y = do
  xx <- eval x
  yy <- eval y

  case (xx, yy) of
    (Number xv, Number yv) ->
      return $ f xv yv

    (xv, yv) ->
        error $ "evaluating non-numbers " ++ show xv ++ " and " ++ show yv


numEq :: AST -> AST -> State Env AST
numEq = numberOp (\x y ->
                        if x == y
                        then Symbol "#t"
                        else Symbol "#f")



numPlus, numMinus, numMult :: AST -> AST -> State Env AST
numPlus = numberOp (\x y -> Number $ x + y)
numMinus = numberOp (\x y -> Number $ x - y)
numMult = numberOp (\x y -> Number $ x * y)

primitives :: [(String, AST -> AST -> State Env AST)]
primitives = [
  ("+", numPlus),
  ("-", numMinus),
  ("*", numMult),
  ("=", numEq)
  ]

fac :: AST
fac = Lambda ["n"]
      (If (List [Symbol "=", Symbol "n", Number 0])
       (Number 1)
       (List [Symbol "*", Symbol "n",
              (List [Symbol "factorial",
                     List [Symbol "-", Symbol "n", Number 1]])]))

type Name = String
type Args = [String]
type Body = [AST]

data AST = Define String AST
         | If AST AST AST
         | Lambda Args AST
         | List [AST]
         | Number Int
         | Symbol String
         deriving (Show, Eq)

eval :: AST -> State Env AST
eval (Symbol s) = do
  env <- get
  case lookup s env of
    Just x -> return x
    Nothing -> return $ Symbol s

eval (If cond true false) = do
  env <- get
  if evalState (eval cond) env == (Symbol "#t")
    then eval true
    else eval false

eval (Define name body) = do
  modify (\env -> (name, body):env)
  return $ Symbol "defined"

eval (List (x:xs)) = do

  h <- eval x
  evaled <- mapM eval xs

  case h of
    (Lambda args body) -> do
          -- TODO - check argument length with function param count
          --   when (length args) /= (length xs) $ do
          --     fail "argument counts don't match"
          let functionEnv = zip args evaled
          env <- get

          return $ evalState (eval body) (functionEnv ++ env)

    (Symbol name) ->
      case lookup name primitives of
        Nothing -> error $ "function " ++ name ++ " is undefined"
        Just f -> f (xs !! 0) (xs !! 1)

eval x = return x -- lambda se vyhodnoti sama na sebe
