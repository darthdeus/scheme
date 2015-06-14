module Scheme.Evaluator where

import Control.Monad.State

import Scheme.Types
import Scheme.Parser
import Scheme.Parser.Combinators

data Lambda = Lambda [String] Body

test = emptyEval (run "((lambda (x) (+ 1 x)) 3)" parseAST)

-- Reprezentuje vyhodnoceny zdrojovy kod, aby bylo mozne prozkoumat
-- jak vysledne hodnoty, tak i hodnoty jednotlivych promennych po volani `define`.
data Evaluated = Evaluated [AST] Env

evaluationResult :: Evaluated -> [AST]
evaluationResult (Evaluated ast _) = ast

instance Show Evaluated where
  show (Evaluated result env) = "Env:\n" ++ show env ++ "\n\n" ++ show result

-- Prazdne prostredi neobsahujici zadne promenne.
emptyEnv :: Env
emptyEnv = []

-- Vyhodnoti zdrojovy kod ve formatu AST
evalSource :: Env -> [AST] -> Evaluated
evalSource env source = uncurry Evaluated $ runState (mapM eval source) env

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

-- Vyhodnoti list jako volani funkce. Prvni hodnota se musi vzdy vyhodnotit bud
-- na atom, nebo na lambda funkci. Pokud se jedna o atom, musi to potom byt
-- vestavena funkce standardni knihovny, protoze uzivatelsky definovane funkce
-- se nahradi za jejich definici (lambda funkci.)
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

    ASTLambda params body -> apply (Lambda (map unwrapAtom params) body) as

    _ -> error $ "Evaluation error, unexpected value in evalList: " ++ show x

-- Slouzi pro vyhodnoceni forem ktere maji specialni vyhodnocovaci pravidla argumentu,
-- pripadne nejaky zasadni vliv na chovani jazyka, jako napr. `define` nebo `let`.
evalSpecialForm :: String -> [AST] -> State Env AST
evalSpecialForm "define" [(ASTAtom name),value] = do
  v <- eval value

  env <- get
  put ((name, v):env)
  return $ ASTAtom "#t"

evalSpecialForm "if" [condition,true,false] = do
  c <- eval condition

  -- Vzdy se vyhodnoti pouze jeden z parametru podminky.
  if c == ASTAtom "#t"
    then eval true
    else eval false

evalSpecialForm "let" [ASTList bindings, body] = do
  b <- mapM evalBindings bindings

  env <- get

  -- Telo formy `let` se vyhodnocuje v upravenem prostredi, ktere obsahuje
  -- nove lokalni promenne, definovane formou let.
  return $ evalState (eval body) (b ++ env)

evalSpecialForm "undef" [ASTAtom name] = do
  -- Aktualni prostredi jednoduse nahradime novym, ktere neobsahuje prislusnou promennou.
  env <- get
  put $ filter (\(x, _) -> x /= name) env

  return $ ASTAtom "undefined"

evalSpecialForm name args = error $ "Invalid special form `" ++ name ++ "` with args: " ++ show args

evalBindings :: AST -> State Env (String, AST)
evalBindings (ASTList [ASTAtom name, value]) = do
  v <- eval value
  return (name, v)

evalBindings x = fail $ "Invalid binding format: " ++ show x

unwrapAtom :: AST -> String
unwrapAtom (ASTAtom name) = name
unwrapAtom err = error $ "Implementation error, expecting atom, found " ++ show err

-- Volani lambda funkce s konkretnimi parametry.
apply :: Lambda -> [AST] -> State Env AST
apply (Lambda params body) values = do
  args <- mapM eval values

  let lambdaEnv = zip params args

  env <- get
  return $ evalState (eval body) (lambdaEnv ++ env)

-- Rozliseni, zda nejaky atom predstavuje nazev specialni formy. Pri pridani nove
-- formy staci pouze zaridit, aby pro jeji nazev tato funkce vracela `True`, a pote
-- pridat prislusny pattern match ve funkci `evalSpecialForm`.
isSpecialForm :: String -> Bool
isSpecialForm "define" = True
isSpecialForm "undef"  = True
isSpecialForm "let"    = True
isSpecialForm "if"     = True
isSpecialForm _ = False

-- Vyhodnoceni vestavene funkce. Jedna se prevazne o funkce ktere nemohou byt uzivatelsky
-- definovane a pracuji na nejnizsi urovni jazyka.
evalBuiltin :: String -> [AST] -> State Env AST
evalBuiltin name as = do
  case lookup name primitives of
    Nothing -> error $ "Atom `" ++ name ++ "` is not a function (neither builtin nor user defined.)"
    Just f -> applyBuiltin f as

-- Reprezentuje vestavenou funkci, aktualne pouze jako funkci dvou promennych.
-- V budoucnu je mozne rozsirit na dalsi arity predelanim z `type` na `data` a upravou prislusnych
-- funkci provadejici vyhodnocovani.
type BuiltinFunction = AST -> AST -> State Env AST

-- Volani vestavene funkce.
applyBuiltin :: BuiltinFunction -> [AST] -> State Env AST
applyBuiltin f as =
  case as of
    [x, y] -> f x y
    _ -> error $ "Invalid number of arguments for builtin function, received: " ++ show as

-- Aplikace obecne funkce pracujici s celociselnymi argumenty s aritou 2.
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

numPlus, numMinus, numMult, numDiv :: AST -> AST -> State Env AST
numPlus = numberOp (\x y -> ASTNumber $ x + y)
numMinus = numberOp (\x y -> ASTNumber $ x - y)
numMult = numberOp (\x y -> ASTNumber $ x * y)
numDiv = numberOp (\x y -> ASTNumber $ x `div` y)

-- Konkretni seznam jednotlivych vestavenych funkci a jejich nazvu.
-- Pro pridani nove funkce staci pridat novy nazev do seznamu, a jeji
-- prislusnou implementaci.
--
-- Aktualne jsou podporovane funkce pouze s aritou 2, ale pridani nove arity
-- je pouze otazka rozsireni typu `BuiltinFunction`, a prislusnych funkci ktere
-- ho pouzivani pri vyhodnocovani.
primitives :: [(String, BuiltinFunction)]
primitives = [
  ("+", numPlus),
  ("-", numMinus),
  ("*", numMult),
  ("/", numDiv),
  ("=", numEq)]
