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
        day15a 65 8921 `shouldBe` 588
