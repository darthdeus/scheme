module Scheme.Types where

-- Prostredi ve kterem se vyhodnocuji vyrazy. Obsahuje vzdy dvojici _nazev_
-- a prislusna hodnota jazyka Scheme. Symboly jsou pote vyhodnoceny na jejich
-- hodnotu v ramci prislusneho prostredi `Env`.
type Env = [(String, AST)]

-- Reprezentuje telo funkce
type Body = AST

-- Hlavni typ reprezentujici abstraktni syntaxi jazyka Scheme.
-- Je nutno zde podotknout, ze prestoze specialni typ pro formu `lambda`
-- neni nutny, a bylo by mozne jej immplementovat pomoci formy `list`, zvolil jsem
-- tuto variantu kvuli prehlednejsim chybovym hlaskam. Dalsi informace jsou dostupne
-- v modulu Evaluator.
data AST = ASTList [AST]
         | ASTLambda [AST] Body
         | ASTNumber Int
         | ASTAtom String
         deriving (Show, Eq)
