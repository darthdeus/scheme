module Scheme.Parser where

import Control.Monad
import qualified Data.Char as C

data Parser a = Parser (String -> [(a, String)])

item :: Parser Char
item = Parser (\s -> case s of
                     "" -> []
                     (c:cs) -> [(c,cs)])

parse :: Parser a -> (String -> [(a, String)])
parse (Parser x) = x

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

-- Obracena varianta $ pro lepsi prehlednost
(|>) :: b -> (b -> c) -> c
(|>) = flip ($)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind x f = Parser g
           where g = (\s -> concatMap m $ parse x s)
                 m = \(a, s') -> parse (f a) s'

satisfies :: (Char -> Bool) -> Parser Char
satisfies f = item `bind` \c ->
              if f c
              then unit c
              else (Parser (\_ -> []))

instance Monad Parser where
   return a = Parser (\s -> [(a,s)])
   p >>= f = Parser (\s -> parse p s |>
                         map (\ (a, s') -> parse (f a) s') |>
                         concat)

instance MonadPlus Parser where
         mzero = Parser (\cs -> [])
         mplus p q = Parser (\s -> (parse p) s ++ (parse q) s)

option :: Parser a -> Parser a -> Parser a
option x y = Parser (\s -> case parse (mplus x y) s of
                                [] -> []
                                (z:_) -> [z])

char :: Char -> Parser Char
char c = satisfies (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = char c >> string cs >> return (c:cs)

many :: Parser a -> Parser [a]
many p = many1 p `option` return []

many1 :: Parser a -> Parser [a]
many1 p = do { a <- p; as <- many p; return (a:as)}

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) `option` return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do a <- p
                    as <- many $ sep >> p
                    return (a:as)

space :: Parser String
space = many (satisfies isSpace)
       where isSpace ' '  = True
             isSpace '\n' = True
             isSpace '\t' = True
             isSpace _    = False

token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

symb :: String -> Parser String
symb s = token (string s)

digit :: Parser Char
digit = satisfies C.isDigit

number :: Parser Int
number = do cs <- many1 digit
            return $ read cs

run :: String -> Int
run s = case parse expr s of
             [(num, _)] -> num
             _ -> -1

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl x op a = (x `chainl1` op) `option` return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 x op = do {a <- x; rest a}
                 where rest a = (do f <- op
                                    b <- x
                                    rest (f a b))
                                `option` return a


-- p :: Parser (Char,Char)
-- p = do
--   c <- item
--   item
--   d <- item
--   return (c, d)


expr :: Parser Int
expr = term `chainl1` addop

term = factor `chainl1` mulop
factor = number `option` do { symb "("; n <- expr; symb ")"; return n}

addop :: Parser (Int -> Int -> Int)
addop = (symb "+" >> return (+)) `option` (symb "-" >> return (-))

mulop :: Parser (Int -> Int -> Int)
mulop = (symb "*" >> return (*)) `option` (symb "/" >> return (div))

data JSONVal = JBool Bool |
               JString String |
               JNumber Int |
               JNull |
               JArray [JSONVal] |
               JPair (String, JSONVal) |
               JObject [(String, JSONVal)]
               deriving (Show)


-- parseBool :: Parser JSONVal
-- parseBool = do { symb "true"; return $ JBool True} `option`
--             do { symb "false"; return $ JBool False }

-- parseString :: Parser JSONVal
-- parseString = do { s <- string; return $ JString s}

-- parseNumber :: Parser JSONVal
-- parseNumber = do { n <- number; return $ JNumber n}

-- parseNull :: Parser JSONVal
-- parseNull = do { symb "null"; return JNull}

-- parseArray :: Parser JSONVal
-- parseArray = do { symb "["; l <- parseJson `sepBy` (symb ","); symb "]"; return $ JArray l}

-- parsePair :: Parser JSONVal
-- parsePair = do { k <- string; symb ":"; v <- parseJson; return $ JPair (k,v)}

-- parseObject :: Parser JSONVal
-- parseObject = do { symb "{"; ps <- many parsePair ; symb "}";
--                    return $ JObject $ map (\ (JPair p) -> p) ps}

-- parseJson :: Parser JSONVal
-- parseJson = parseBool `option` parseString `option` parseNumber `option`
--             parseNull `option` parseArray `option` parseObject
