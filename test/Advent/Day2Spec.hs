{-# Language OverloadedStrings #-}
module Advent.Day2Spec (spec) where
  import Test.Hspec
  import Advent.Day2 (day2a, day2b, difference)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day2a" $ do
      describe "difference" $ do
        it "find difference" $ do
          difference "5 1 9 5" `shouldBe` 8
        it "find difference" $ do
          difference "7 5 3" `shouldBe` 4
        it "find difference" $ do
          difference "2 4 6 8" `shouldBe` 6
