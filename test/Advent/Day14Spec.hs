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
        pendingWith "to get fast feedback test is temporary disabled"
        day14a "flqrgnkx" `shouldBe` 8108
    describe "day14b" $ do
      it "calculates regions" $ do
        pendingWith "to get fast feedback test is temporary disabled"
        day14b "flqrgnkx" `shouldBe` 1242
