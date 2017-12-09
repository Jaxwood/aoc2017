module Advent.Day5Spec (spec) where

  import Test.Hspec
  import Advent.Day5 (day5a, day5b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day5a" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day5.csv")
        day5a csv  `shouldBe` 5
    describe "day5b" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day5.csv")
        day5b csv  `shouldBe` 10
