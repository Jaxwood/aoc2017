module Advent.Day06Spec (spec) where

  import Test.Hspec
  import Advent.Day06 (day6a, day6b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day6a" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day6.csv")
        day6a csv  `shouldBe` 5
    describe "day6b" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day6.csv")
        day6b csv  `shouldBe` 4
