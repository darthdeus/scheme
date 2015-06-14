--- Implementace vlastniho Scheme parseru za pomoci parsovacich kombinatoru.
module Scheme.Parser where

import Scheme.Types
import Scheme.Parser.Combinators

-- Parser pro jeden prvek AST v Scheme.
parseAST :: Parser AST
parseAST = parseDefine <|>
           parseIf <|>
           parseLet <|>
           parseLambda <|>
           parseList <|>
           parseNumber <|>
           parseAtom

parseDefine = fmap Atom $ string "TODO-define"
parseLambda = fmap Atom $ string "TODO-lambda"
parseLet = fmap Atom $ string "TOOD-let"

parseIf :: Parser AST
parseIf = do
  values <- specialForm "if"

  case values of
    [cond,true,false] ->
      return $ If cond true false

    _ -> failed

whitespace :: Parser String
whitespace = many $ oneOf " \n\t\r"

specialForm :: String -> Parser [AST]
specialForm name = do
  (List values) <- parseList

  case values of
    ((Atom x):xs) -> do
      if x == name
        then return xs
        else failed

    _ -> failed

parseAtom :: Parser AST
parseAtom = fmap Atom $ many1 $ oneOf "abcdefghijklmnopqrstuvwxyz!@#$%^&*_+-=[]{}\\|';:\",./<>?"

parseNumber :: Parser AST
parseNumber = fmap Number number

parseList :: Parser AST
parseList = fmap List $ bracket '(' ')' (sepby parseAST whitespace)
