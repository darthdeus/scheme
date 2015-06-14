--- Implementace vlastniho Scheme parseru za pomoci parsovacich kombinatoru.
module Scheme.Parser where

import Scheme.Types
import Scheme.Parser.Combinators

-- Parser pro jeden prvek AST v Scheme.
parseAST :: Parser AST
parseAST = parseList <|>
           parseNumber <|>
           parseAtom

parseDefine = fmap Atom $ string "TODO-define"

parseLambda :: Parser AST
parseLambda = do
  values <- specialForm "lambda"

  case values of
    [bindings, body] -> return $ Lambda bindings body

    _ -> failed $ "the `lambda` form expects exactly two values, found: " ++ show values


parseLet :: Parser AST
parseLet = do
  values <- specialForm "let"

  case values of
    [bindings, body] -> return $ Let bindings body

    _ -> failed $ "the `let` form expects exactly two values, found: " ++ show values

parseIf :: Parser AST
parseIf = do
  values <- specialForm "if"

  case values of
    [cond,true,false] ->
      return $ If cond true false

    _ -> failed $ "the `if` form expects exactly three branches, found: " ++ show values

whitespace :: Parser String
whitespace = many $ oneOf " \n\t\r"

specialForm :: String -> Parser [AST]
specialForm name = do
  (List values) <- parseList

  case values of
    ((Atom x):xs) -> do
      if x == name
        then return xs
        else failed $ "expected special form `" ++ name ++ "`, found `" ++ x ++ "` instead"

    _ -> failed $ "expected special form, found: " ++ show values

parseAtom :: Parser AST
parseAtom = fmap Atom $ many1 $ oneOf "abcdefghijklmnopqrstuvwxyz!@#$%^&*_+-=[]{}\\|';:\",./<>?"

parseNumber :: Parser AST
parseNumber = fmap Number number

parseList :: Parser AST
parseList = fmap List $ bracket '(' ')' (sepby parseAST whitespace)
