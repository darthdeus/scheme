{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec, main) where

import Scheme.Parser
import Test.Hspec

main :: IO ()
main = hspec spec

matchRight :: (Show a, Eq a) => Maybe a -> a -> IO ()
matchRight ex y = case ex of
    Nothing -> fail "parsing failed"
    Just x -> x `shouldBe` y

spec :: Spec
spec = do
  it "parses a char" $
    run "c" (char 'c') `matchRight` 'c'

  it "parses a string" $ do
    run "hello world" (string "hello") `matchRight` "hello"

  it "parses a separated list of ints" $ do
    run "1,2,3" (sepby1 int (char ',')) `matchRight` [1,2,3]
