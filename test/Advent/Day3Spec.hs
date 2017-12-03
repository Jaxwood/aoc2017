module Advent.Day3Spec (spec) where

  import Test.Hspec
  import Advent.Day3 (day3a, day3b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day3a" $ do
      it "1 is 0 steps away" $ do
        day3a 1 `shouldBe` 0
      it "12 is 0 steps away" $ do
        day3a 12 `shouldBe` 3
      it "1 is 0 steps away" $ do
        day3a 23 `shouldBe` 2
      it "1 is 0 steps away" $ do
        day3a 1024 `shouldBe` 31
