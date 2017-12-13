module Advent.Day7Spec (spec) where

  import Test.Hspec
  import Advent.Day7 (day7a, day7b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day7a" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day7.csv")
        day7a csv  `shouldBe` ""
    describe "day7b" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day7.csv")
        day7b csv  `shouldBe` ""
