module Advent.Day14Spec (spec) where

  import Test.Hspec
  import Advent.Day14 (day14a, day14b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day14a" $ do
      it "calculates used spaces" $ do
        day14a "flqrgnkx" `shouldBe` 8108
