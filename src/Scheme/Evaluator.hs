{-# LANGUAGE ScopedTypeVariables #-}
module Scheme.Evaluator where

import Data.Functor.Identity
import Control.Monad.State

import Scheme.Types
import Scheme.Parser
import Scheme.Parser.Combinators

import Debug.Trace


data Lambda = Lambda [String] Body

-- -- Testovaci implementace factorialu, ekvivalentni nasledujicimu kodu
-- --
-- -- (define factorial
-- --           (lambda (n)
-- --              (if (= n 0)
-- --                1
-- --                (* n (factorial (- n 1)))))
-- --
-- --
-- -- TODO - tohle extrahovat jinam
-- -- (factorial 5)
-- -- ((lambda (x) (+ 1 x)) 3)
-- fac :: AST
-- fac = Lambda ["n"]
--       (If (List [Atom "=", Atom "n", Number 0])
--        (Number 1)
--        (List [Atom "*", Atom "n",
--               (List [Atom "factorial",
--                      List [Atom "-", Atom "n", Number 1]])]))

test = emptyEval (run "((lambda (x) (+ 1 x)) 3)" parseAST)

data Evaluated = Evaluated [AST] Env

evaluationResult :: Evaluated -> [AST]
evaluationResult (Evaluated ast _) = ast

instance Show Evaluated where
  show (Evaluated result env) = "Env:\n" ++ show env ++ "\n\n" ++ show result

emptyEnv :: Env
emptyEnv = []

-- Vyhodnoti zdrojovy kod ve formatu AST
evalSource :: [AST] -> Evaluated
evalSource source = uncurry Evaluated $ runState (mapM eval source) emptyEnv

-- Vse krome listu se vyhodnocuje samo na sebe (cisla a atomy).
eval :: AST -> State Env AST
eval (ASTList list) = evalList list
eval (ASTAtom name) = do
  env <- get

  case lookup name env of
    Just value -> return value
    Nothing -> return $ ASTAtom name

eval (ASTNumber n) = return $ ASTNumber n
eval x = return x


evalWithEnv :: AST -> Env -> AST
evalWithEnv x env = evalState (eval x) env

emptyEval :: AST -> AST
emptyEval x = evalState (eval x) emptyEnv

evalList :: [AST] -> State Env AST
evalList [] = error "Empty list can't be evaluated as a function"
evalList (a:as) = do
  -- Ve vsech pripadech chceme vyhodnotit hlavu. Tim se umozni
  -- napr. kod ((if x define lambda) y z), kde se vybere specialni forma
  -- podle vyhodnoceni prvniho vyrazu.
  x <- eval a

  -- Dalsi vyhodnocovani ale uz velmi zalezi na typu hlavy
  case x of
    -- Cislo nemuze byt nikdy funkce, proto se jedna o chybu
    ASTNumber _ -> error "Number can't be evaluated as a function"

    -- Pokud byl symbol v puvodni hlave, a znaci uzivatelsky definovanou funkci,
    -- bude v tuto chvili jiz nahrazeny za lambda funkci (jeho definici.) Pokud
    -- ale i po vyhodnoceni mame porad symbol, muze se jednat jedine o specialni
    -- formu, pro kterou musime vyhodnocovat argumenty zvlast.
    ASTAtom name ->
      if isSpecialForm name
      then evalSpecialForm name as
      else evalBuiltin name as

    -- TODO - fix args vs params
    ASTLambda params body -> apply (Lambda (map unwrapAtom params) body) as

    _ -> error $ "Evaluation error, unexpected value in evalList: " ++ show x

evalSpecialForm :: String -> [AST] -> State Env AST
evalSpecialForm "define" [(ASTAtom name),value] = do
  env <- get
  put ((name, value):env)
  return $ ASTAtom "#t"

unwrapAtom :: AST -> String
unwrapAtom (ASTAtom name) = name
unwrapAtom err = error $ "Implementation error, expecting atom, found " ++ show err

apply :: Lambda -> [AST] -> State Env AST
apply (Lambda params body) values = do
  args <- mapM eval values

  let lambdaEnv = zip params args

  env <- get
  return $ evalState (eval body) (lambdaEnv ++ env)

isSpecialForm :: String -> Bool
isSpecialForm "define" = True
isSpecialForm "lambda" = True
isSpecialForm "apply"  = True
isSpecialForm "let"    = True
isSpecialForm "if"     = True
isSpecialForm _ = False

evalBuiltin :: String -> [AST] -> State Env AST
evalBuiltin name as = do
  case lookup name primitives of
    Nothing -> error $ "Atom `" ++ name ++ "` is not a function (neither builtin nor user defined.)"
    Just f -> applyBuiltin f as

type BuiltinFunction = AST -> AST -> State Env AST

applyBuiltin :: BuiltinFunction -> [AST] -> State Env AST
applyBuiltin f as =
  case as of
    [x, y] -> f x y
    _ -> error $ "Invalid number of arguments for builtin function, received: " ++ show as

numberOp :: (Int -> Int -> AST) -> AST -> AST -> State Env AST
numberOp f x y = do
  xx <- eval x
  yy <- eval y

  case (xx, yy) of
    (ASTNumber xv, ASTNumber yv) ->
      return $ f xv yv

    (xv, yv) ->
        error $ "evaluating non-numbers " ++ show xv ++ " and " ++ show yv


numEq :: AST -> AST -> State Env AST
numEq = numberOp (\x y ->
                        if x == y
                        then ASTAtom "#t"
                        else ASTAtom "#f")

numPlus, numMinus, numMult :: AST -> AST -> State Env AST
numPlus = numberOp (\x y -> ASTNumber $ x + y)
numMinus = numberOp (\x y -> ASTNumber $ x - y)
numMult = numberOp (\x y -> ASTNumber $ x * y)

primitives :: [(String, BuiltinFunction)]
primitives = [
  ("+", numPlus),
  ("-", numMinus),
  ("*", numMult),
  ("=", numEq)]

-- eval :: AST -> State Env AST
-- eval (Atom s) = do
--   env <- get
--   case lookup s env of
--     Just x -> return x
--     Nothing -> return $ Atom s

-- eval (If cond true false) = do
--   env <- get
--   if evalState (eval cond) env == (Atom "#t")
--     then eval true
--     else eval false

-- eval (Define name body) = do
--   modify (\env -> (name, body):env)
--   return $ Atom "defined"

-- eval (List (x:xs)) = do

--   h <- eval x
--   evaled <- mapM eval xs

--   case h of
--     (Lambda args body) -> do
--           -- TODO - check argument length with function param count
--           --   when (length args) /= (length xs) $ do
--           --     fail "argument counts don't match"
--           let functionEnv = zip args evaled
--           env <- get

--           return $ evalState (eval body) (functionEnv ++ env)

--     (Atom name) ->
--       case lookup name primitives of
--         Nothing -> error $ "function " ++ name ++ " is undefined"
--         Just f -> f (xs !! 0) (xs !! 1)

-- eval x = return x -- lambda se vyhodnoti sama na sebe


-- -- Reprezentuje jeden prvek AST v Scheme. Cely zdrojovy kod
-- -- je potom reprezentovan jako [AST].
-- data Form = Define String AST
--           | If AST AST AST
--           | Let AST AST
--           | Lambda Args AST
--           | List [AST]
--           | Number Int
--           | Atom String
--           deriving (Show, Eq)
