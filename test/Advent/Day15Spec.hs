module Advent.Day15Spec (spec) where

  import Test.Hspec
  import Advent.Day15 (day15a, day15b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day15a" $ do
      it "get pairs" $ do
        pendingWith "slow test"
        day15a 65 8921 `shouldBe` 588
    describe "day15b" $ do
      it "get pairs" $ do
        pendingWith "slow test"
        day15b 65 8921 `shouldBe` 309
