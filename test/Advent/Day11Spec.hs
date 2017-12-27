module Advent.Day11Spec (spec) where

  import Test.Hspec
  import Advent.Day11 (day11a, day11b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day11a" $ do
      it "ne,ne,ne" $ do
        day11a "ne,ne,ne" `shouldBe` 3
      it "ne,ne,sw,sw" $ do
        day11a "ne,ne,sw,sw" `shouldBe` 0
      it "ne,ne,s,s" $ do
        day11a "ne,ne,s,s" `shouldBe` 2
      it "se,sw,se,sw,sw" $ do
        day11a "se,sw,se,sw,sw" `shouldBe` 3
