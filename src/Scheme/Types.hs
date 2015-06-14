module Scheme.Types where

import Control.Monad
import Control.Monad.State

type Env = [(String, AST)]

type Name = String
type Args = [AST]
type Body = AST

data AST = ASTList [AST]
         | ASTLambda Args Body
         | ASTNumber Int
         | ASTAtom String
         deriving (Show, Eq)

data Lambda = Lambda Args Body

emptyEnv :: Env
emptyEnv = []

evalSource :: [AST] -> [AST]
evalSource source = evalState (evalWithEnv source) emptyEnv

evalWithEnv :: [AST] -> State Env [AST]
evalWithEnv [] = return []
evalWithEnv (a:as) = do
  x <- eval a
  xs <- evalWithEnv as

  return (x:xs)

-- Vse krome listu se vyhodnocuje samo na sebe (cisla a atomy).
eval :: AST -> State Env AST
eval (ASTList list) = evalList list
eval x = return x

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
      else error $ "Atom `" ++ name ++ "` is not a function (neither builtin nor user defined.)"

    ASTList [(ASTAtom "lambda"),(ASTList args),body] -> apply (Lambda args body) as

    _ -> error $ "Evaluation error, unexpected value in evalList: " ++ show x

evalSpecialForm :: Name -> [AST] -> State Env AST
evalSpecialForm = undefined

apply :: Lambda -> [AST] -> State Env AST
apply (Lambda args body) values = do
  params <- mapM eval values

  undefined


isSpecialForm :: String -> Bool
isSpecialForm "define" = True
isSpecialForm "lambda" = True
isSpecialForm "apply"  = True
isSpecialForm "let"    = True
isSpecialForm "if"     = True
isSpecialForm _ = False


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

-- numberOp :: (Int -> Int -> AST) -> AST -> AST -> State Env AST
-- numberOp f x y = do
--   xx <- eval x
--   yy <- eval y

--   case (xx, yy) of
--     (Number xv, Number yv) ->
--       return $ f xv yv

--     (xv, yv) ->
--         error $ "evaluating non-numbers " ++ show xv ++ " and " ++ show yv


-- numEq :: AST -> AST -> State Env AST
-- numEq = numberOp (\x y ->
--                         if x == y
--                         then Atom "#t"
--                         else Atom "#f")



-- numPlus, numMinus, numMult :: AST -> AST -> State Env AST
-- numPlus = numberOp (\x y -> Number $ x + y)
-- numMinus = numberOp (\x y -> Number $ x - y)
-- numMult = numberOp (\x y -> Number $ x * y)

-- primitives :: [(String, AST -> AST -> State Env AST)]
-- primitives = [
--   ("+", numPlus),
--   ("-", numMinus),
--   ("*", numMult),
--   ("=", numEq)]


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
