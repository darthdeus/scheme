-- Zakladni parser kombinatory, implementovano na zaklade
-- clanku `Monadic Parsing in Haskell` https://www.cs.nott.ac.uk/~gmh/pearl.pdf,
-- s drobnym vylepsenim, kde v puvodnim clanku se pouzivalo nedeterministicke
-- `[(a, String)]` pro reseni chybovych chlasek. Tento parser pouziva misto toho
-- `Either String (a, String)`, aby bylo mozne s chybou dale prenaset i proc parsovani
-- selhalo.
module Scheme.Parser.Combinators where

import Control.Applicative as A hiding (many, (<|>))
import Control.Monad

data Parser a = Parser (String -> ParseResult a)
type ParseResult a = Either String (a, String)

-- TODO - rewrite using Monad instance
instance Functor Parser where
  fmap f (Parser p) = Parser (\x -> do
                                 case p x of
                                   Left err -> Left err
                                   Right (a, s) -> Right (f a, s))

-- TODO - rewrite using Monad instance
instance Applicative Parser where
  pure x = Parser (\s -> Right (x, s))
  (Parser f) <*> (Parser g) = Parser (\x -> do -- pozor, tento `do` je v Either monade
                                         (a, s) <- f x
                                         (b, t) <- g s
                                         return $ (a b, t))

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser (\x -> do -- pozor, tento `do` je v Either monade
                                (a, s) <- p x
                                let (Parser q) = f a
                                q s)

parse :: Parser a -> String -> ParseResult a
parse (Parser p) s = p s

failed :: String -> Parser a
failed why = Parser (\_ -> Left why)

instance MonadPlus Parser where
  mzero = failed "mzero: parsing failed"
  mplus p q = Parser (\x ->
                       case parse p x of
                         Right a -> Right a
                         Left err -> parse q x)


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

run :: String -> Parser a -> a
run s (Parser f) = case f s of
  Left err -> error err
  Right (a, _) -> a

anyChar :: Parser Char
anyChar = Parser (\s -> case s of
                     "" -> Left "expected a character, empty string instead"
                     (x:xs) -> Right (x, xs))

satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  c <- anyChar

  if p c
    then return c
    else failed $ "unexpected char: " ++ [c]

char :: Char -> Parser Char
char c = satisfies (c ==)


-- High level parser combinators
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

-- TODO - pochopit :)
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                   rest a = (do f <- op
                                b <- p
                                rest (f a b))
                            <|> return a

-- Parsuje vyraz uzavorkovany zavorkami
bracket :: Char -> Char -> Parser a -> Parser a
bracket left right middle = do
  char left
  m <- middle
  char right
  return m

oneOf :: String -> Parser Char
oneOf s = oneOfAcc s s
  where oneOfAcc what [] = failed $ "none of the characters matched: " ++ what
        oneOfAcc what (x:xs) = do
          char x <|> oneOfAcc what xs

digit :: Parser Int
digit = do
  c <- oneOf "0123456789"
  return $ read [c]

number :: Parser Int
number = do
  digits <- many1 digit
  return $ foldl (\acc x -> acc * 10 + x) 0 digits
