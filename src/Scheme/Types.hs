module Scheme.Types where

type Env = [(String, AST)]

type Body = AST

data AST = ASTList [AST]
         | ASTLambda [AST] Body
         | ASTNumber Int
         | ASTAtom String
         deriving (Show, Eq)
