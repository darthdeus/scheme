-- Zakladni parser kombinatory, implementovano na zaklade
-- clanku `Monadic Parsing in Haskell` https://www.cs.nott.ac.uk/~gmh/pearl.pdf
module Scheme.Parser.Combinators where

import Control.Applicative as A hiding (many, (<|>))
import Control.Monad

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
                                 [(f a, s)])

instance Applicative Parser where
  pure x = Parser (\s -> [(x, s)])
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

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance MonadPlus Parser where
  mzero = Parser (\_ -> [])
  mplus p q = Parser (\x -> parse p x ++ parse q x)

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser (\s -> case parse (p `mplus` q) s of
                     [] -> []
                     (x:_) -> [x])

run :: String -> Parser a -> Maybe a
run s (Parser f) = case f s of
  [] -> Nothing
  ((x, _):_) -> Just x

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
string (x:xs) = char x >> string xs >> return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p s = (p `sepby1` s) <|> return []

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p s = do
  a <- p
  as <- many $ s >> p
  return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                   rest a = (do f <- op
                                b <- p
                                rest (f a b))
                            <|> return a

bracket :: Char -> Char -> Parser a -> Parser a
bracket left right middle = do
  char left
  m <- middle
  char right
  return m

oneOf :: String -> Parser Char
oneOf [] = failed
oneOf (x:xs) = do
  char x <|> oneOf xs

digit :: Parser Int
digit = do
  c <- oneOf "0123456789"
  return $ read [c]

number :: Parser Int
number = do
  digits <- many1 digit
  return $ foldl (\acc x -> acc * 10 + x) 0 digits