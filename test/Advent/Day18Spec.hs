module Advent.Day18Spec (spec) where

  import Test.Hspec
  import System.Directory
  import Advent.Day18 (day18a, day18b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day18a" $ do
      it "should find the last sound played" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day18.csv")
        day18a csv `shouldBe` 4
    describe "day18b" $ do
      it "TBD" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day18.csv")
        day18b csv `shouldBe` 0
