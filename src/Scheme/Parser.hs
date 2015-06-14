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
parseIf = fmap Atom $ string "TOOD-if"

whitespace :: Parser String
whitespace = many $ oneOf " \n\t\r"

parseAtom :: Parser AST
parseAtom = fmap Atom $ many1 $ oneOf "abcdefghijklmnopqrstuvwxyz!@#$%^&*_+-=[]{}\\|';:\",./<>?"

parseNumber :: Parser AST
parseNumber = fmap Number number

parseList :: Parser AST
parseList = fmap List $ bracket '(' ')' (sepby parseAST whitespace)
