module Advent.Day17Spec (spec) where

  import Test.Hspec
  import Advent.Day17 (day17a, day17b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day17a" $ do
      it "should find the value after 2017" $ do
        day17a 3 `shouldBe` 638
