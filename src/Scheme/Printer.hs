module Scheme.Printer where

import Scheme.Types
import Data.List

printLisp :: AST -> String
printLisp (ASTAtom name) = name
printLisp (ASTNumber number) = show number
printLisp (ASTLambda args body) = "(lambda (" ++ argsString ++ ") " ++ bodyString ++ ")"
  where argsString = intercalate " " $ map printLisp args
        bodyString = printLisp body
printLisp (ASTList body) = "(" ++ intercalate " " (map printLisp body) ++ ")"
