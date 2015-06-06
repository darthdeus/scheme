module Scheme.Types where

import Control.Monad
import Control.Monad.State

type Env = [(String, Form)]

data Form = Symbol String
          | List [Form]
            deriving (Show, Eq)

eval :: Form -> State Env Form
eval (Symbol sym) = return $ Symbol sym
eval (List (x:xs)) = do
  (Symbol h) <- eval x
  args <- mapM eval xs

  case h of
    "define" -> evalDefine (Symbol h) args
    _ -> return (Symbol h)

evalDefine :: Form -> [Form] -> State Env Form
evalDefine = undefined
