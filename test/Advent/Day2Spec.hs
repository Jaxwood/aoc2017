module Advent.Day2Spec (spec) where

  import Test.Hspec
  import Advent.Day2 (day2a, day2b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day2a" $ do
      it "find the sum of rows" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day2.csv")
        day2a csv `shouldBe` 18
    describe "day2b" $ do
      it "find evenly dividable number" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day2b.csv")
        day2b csv `shouldBe` 9
