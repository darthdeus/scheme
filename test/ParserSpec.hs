{-# LANGUAGE OverloadedStrings #-}
module ParserSpec (spec, main) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Scheme parser" $
        it "works" $
            True `shouldBe` True
