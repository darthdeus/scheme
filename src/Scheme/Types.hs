module Scheme.Types where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.State

type Env = [(String, AST)]

type Name = String
type Args = [String]
type Body = [AST]

-- Reprezentuje jeden prvek AST v Scheme. Cely zdrojovy kod
-- je potom reprezentovan jako [AST].
data AST = Define String AST
         | If AST AST AST
         | Let AST AST
         | Lambda Args AST
         | List [AST]
         | Number Int
         | Atom String
         deriving (Show, Eq)

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
                        then Atom "#t"
                        else Atom "#f")



numPlus, numMinus, numMult :: AST -> AST -> State Env AST
numPlus = numberOp (\x y -> Number $ x + y)
numMinus = numberOp (\x y -> Number $ x - y)
numMult = numberOp (\x y -> Number $ x * y)

primitives :: [(String, AST -> AST -> State Env AST)]
primitives = [
  ("+", numPlus),
  ("-", numMinus),
  ("*", numMult),
  ("=", numEq)]


-- Testovaci implementace factorialu, ekvivalentni nasledujicimu kodu
--
-- (define factorial
--           (lambda (n)
--              (if (= n 0)
--                1
--                (* n (factorial (- n 1)))))
--
--
-- TODO - tohle extrahovat jinam
-- (factorial 5)
-- ((lambda (x) (+ 1 x)) 3)
fac :: AST
fac = Lambda ["n"]
      (If (List [Atom "=", Atom "n", Number 0])
       (Number 1)
       (List [Atom "*", Atom "n",
              (List [Atom "factorial",
                     List [Atom "-", Atom "n", Number 1]])]))

eval :: AST -> State Env AST
eval (Atom s) = do
  env <- get
  case lookup s env of
    Just x -> return x
    Nothing -> return $ Atom s

eval (If cond true false) = do
  env <- get
  if evalState (eval cond) env == (Atom "#t")
    then eval true
    else eval false

eval (Define name body) = do
  modify (\env -> (name, body):env)
  return $ Atom "defined"

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

    (Atom name) ->
      case lookup name primitives of
        Nothing -> error $ "function " ++ name ++ " is undefined"
        Just f -> f (xs !! 0) (xs !! 1)

eval x = return x -- lambda se vyhodnoti sama na sebe
