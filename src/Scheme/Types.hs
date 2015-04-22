module Scheme.Types where

data Form = Symbol String
          | String String
          | Number Integer
          | Bool Bool
          | List [From]
