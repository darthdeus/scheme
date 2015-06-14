--- Implementace vlastniho Scheme parseru za pomoci parsovacich kombinatoru.
module Scheme.Parser where

import Scheme.Types
import Scheme.Parser.Combinators

-- Parsuje z daneho stringu kompletni kod jazyka Scheme, ktery muze byt
-- rozdeleny i na vice radku.
parseLisp :: String -> [AST]
parseLisp s = run s $ many1 (whitespace >> parseAST)

-- Parser pro jeden prvek AST v Scheme.
parseAST :: Parser AST
parseAST = whitespace >>
           (parseLambda <|>
            parseList <|>
            parseNumber <|>
            parseAtom)

-- Parsuje libovolne mnozstvi prazdnych znaku.
whitespace :: Parser String
whitespace = many $ oneOf " \n\t\r"

-- Parsuje atom jazyka Scheme.
parseAtom :: Parser AST
parseAtom = fmap ASTAtom $ many1 $ oneOf "abcdefghijklmnopqrstuvwxyz!@#$%^&*_+-=[]{}\\|';:\",./<>?"

-- Parsuje cele cislo.
parseNumber :: Parser AST
parseNumber = fmap ASTNumber number

-- Parsuje list, ktery muze obsahovat libovolne dalsi hodnoty jazyka Scheme.
parseList :: Parser AST
parseList = fmap ASTList $ bracket '(' ')' (sepby parseAST whitespace)

-- Parsuje lambda funkci. Tento parser slouzi prevazne pro zprehledneni chybovych
-- hlasek, kde pokud by se lambda funkce vyhodnocovaly jako _list_, nebylo by jiz mozne
-- tak presne zobrazit syntakticke chyby.
parseLambda :: Parser AST
parseLambda = do
  values <- specialForm "lambda"

  case values of
    [ASTList args, body] ->
      if all isAtom args
        then return $ ASTLambda args body
        else failed $
             "Lambda arguments can be only atoms, found " ++ show args
    _ -> failed $ "Unexpected lambda format, found: " ++ show values

isAtom :: AST -> Bool
isAtom (ASTAtom _) = True
isAtom _ = False

-- Parsuje specialni formu s danym nazvem (prvni prvek listu musi byt atom s nazvem
-- dane formy, ostatni mohou byt libovolne.) Opet slouzi prevazne pro zlepseni chybovych
-- hlasek.
specialForm :: String -> Parser [AST]
specialForm name = do
  (ASTList values) <- parseList

  case values of
    ((ASTAtom x):xs) -> do
      if x == name
        then return xs
        else failed $ "expected special form `" ++ name ++ "`, found `" ++ x ++ "` instead"

    _ -> failed $ "expected special form, found: " ++ show values
