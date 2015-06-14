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

parseAtom :: Parser AST
parseAtom = oneOf "abcdefghijklmnopqrstuvwxyz-_#@*"
