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

-- Implementace pro Functor a Applicative slouzi pouze pro kompatibilitu
-- s GHC Haskellem 7.10, kde je kazda Monada musi implementovat.
instance Functor Parser where
  fmap f (Parser p) = Parser (\x -> do
                                 case p x of
                                   Left err -> Left err
                                   Right (a, s) -> Right (f a, s))

instance Applicative Parser where
  pure = return
  (Parser f) <*> (Parser g) = Parser (\x -> do -- pozor, tento `do` je v Either monade
                                         (a, s) <- f x
                                         (b, t) <- g s
                                         return $ (a b, t))

instance Monad Parser where
  return x = Parser (\s -> Right (x, s))
  (Parser p) >>= f = Parser (\x -> do -- pozor, tento `do` je v Either monade
                                (a, s) <- p x
                                let (Parser q) = f a
                                q s)

-- Implementace instance MonadPlus je prevazne pro ilustrativni ucely.
instance MonadPlus Parser where
  mzero = failed "mzero: parsing failed"
  mplus p q = Parser (\x ->
                       case parse p x of
                         Right a -> Right a
                         Left _ -> parse q x)

-- Aplikuje parser na dany vstup.
parse :: Parser a -> String -> ParseResult a
parse (Parser p) s = p s

-- Vytvori parser ktery vzdy selze pro jakykoliv vstup, pricemz chybova hlaska
-- je dana prvnim parametrem.
failed :: String -> Parser a
failed why = Parser (\_ -> Left why)

-- Operator `or`, ktery napred zkusi pouzit prvni parser, a pokud selze
-- tak pouzije druhy parser.
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus

run :: String -> Parser a -> a
run s (Parser f) = case f s of
  Left err -> error err
  Right (a, _) -> a

-- Parsuje libovolny znak
anyChar :: Parser Char
anyChar = Parser (\s -> case s of
                     "" -> Left "expected a character, empty string instead"
                     (x:xs) -> Right (x, xs))

-- Vytvori parser pro jeden znak, ktery splnuje dany predikat.
satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
  c <- anyChar

  if p c
    then return c
    else failed $ "unexpected char: " ++ [c]

-- Parser daneho znaku.
char :: Char -> Parser Char
char c = satisfies (c ==)

-- Parser libovolneho retezce.
string :: String -> Parser String
string "" = return ""
string (x:xs) = char x >> string xs >> return (x:xs)

-- Parsuje libovolny pocet hodnot daneho parseru (muze byt i 0)
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- Podobne jako `many`, ale parsuje alespon jednu hodnotu.
many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

-- Parsuje hodnoty dane prvnim parserem, oddelene odelovaci podle druheho parseru,
-- napr. `sepby digit (char ',')` parsuje string "1,2,3" jako list [1,2,3].
sepby :: Parser a -> Parser b -> Parser [a]
sepby p s = (p `sepby1` s) <|> return []

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p s = do
  a <- p
  as <- many $ s >> p
  return (a:as)

-- Parsuje vyraz uzavorkovany zavorkami
bracket :: Char -> Char -> Parser a -> Parser a
bracket left right middle = do
  char left
  m <- middle
  char right
  return m

-- Parsuje libovolny znak z daneho stringu.
oneOf :: String -> Parser Char
oneOf s = oneOfAcc s s
  where oneOfAcc what [] = failed $ "none of the characters matched: " ++ what
        oneOfAcc what (x:xs) = do
          char x <|> oneOfAcc what xs

-- Parsuje jednu cislici.
digit :: Parser Int
digit = do
  c <- oneOf "0123456789"
  return $ read [c]

number :: Parser Int
number = do
  digits <- many1 digit
  return $ foldl (\acc x -> acc * 10 + x) 0 digits
