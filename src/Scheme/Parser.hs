module Scheme.Parser where

import Control.Applicative
import Control.Monad
import qualified Data.Char as C

data Parser a = Parser (String -> [(a, String)])

runParser :: Parser a -> (String -> [(a, String)])
runParser (Parser p) = p

anyChar :: Parser Char
anyChar = Parser (\s -> case s of
                     "" -> []
                     (x:xs) -> [(x, xs)])

failed :: Parser a
failed = Parser (\_ -> [])

instance Functor Parser where
  fmap f (Parser p) = Parser (\x -> do
                                 (a, s) <- p x
                                 Just (f a, s))

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (Parser f) <*> (Parser g) = Parser (\x -> do
                                         (a, s) <- f x
                                         (b, t) <- g s

                                         return $ (a b, t))

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser (\x -> do
                                (a, s) <- p x
                                let (Parser q) = f a
                                q s)

run :: String -> Parser a -> Maybe a
run s (Parser f) = case f s of
  Just (x, _) -> Just x
  Nothing -> Nothing

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  c <- anyChar

  if p c
    then return c
    else failed

char :: Char -> Parser Char
char c = satisfies (c ==)

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
  y <- char x
  ys <- string xs
  return (y:ys)

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser (\x ->
                      case runParser p x of
                        Just res -> return res
                        Nothing -> runParser q x)
infixr 1 <|>

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)
