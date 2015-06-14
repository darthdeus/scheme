--- Implementace vlastniho Scheme parseru za pomoci parsovacich kombinatoru.
module Scheme.Parser where

import Scheme.Types
import Scheme.Parser.Combinators


parseLisp :: String -> [AST]
parseLisp s = run s $ many1 parseAST

-- Parser pro jeden prvek AST v Scheme.
parseAST :: Parser AST
parseAST = parseLambda <|>
           parseList <|>
           parseNumber <|>
           parseAtom

whitespace :: Parser String
whitespace = many $ oneOf " \n\t\r"

parseAtom :: Parser AST
parseAtom = fmap ASTAtom $ many1 $ oneOf "abcdefghijklmnopqrstuvwxyz!@#$%^&*_+-=[]{}\\|';:\",./<>?"

parseNumber :: Parser AST
parseNumber = fmap ASTNumber number

parseList :: Parser AST
parseList = fmap ASTList $ bracket '(' ')' (sepby parseAST whitespace)

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

specialForm :: String -> Parser [AST]
specialForm name = do
  (ASTList values) <- parseList

  case values of
    ((ASTAtom x):xs) -> do
      if x == name
        then return xs
        else failed $ "expected special form `" ++ name ++ "`, found `" ++ x ++ "` instead"

    _ -> failed $ "expected special form, found: " ++ show values

-- parseLambda :: Parser AST
-- parseLambda = do
--   values <- specialForm "lambda"

--   case values of
--     [bindings, body] -> return $ Lambda bindings body

--     _ -> failed $ "the `lambda` form expects exactly two values, found: " ++ show values


-- parseLet :: Parser AST
-- parseLet = do
--   values <- specialForm "let"

--   case values of
--     [bindings, body] -> return $ Let bindings body

--     _ -> failed $ "the `let` form expects exactly two values, found: " ++ show values

-- parseIf :: Parser AST
-- parseIf = do
--   values <- specialForm "if"

--   case values of
--     [cond,true,false] ->
--       return $ If cond true false

--     _ -> failed $ "the `if` form expects exactly three branches, found: " ++ show values
