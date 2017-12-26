module Advent.Day10Spec (spec) where

  import Test.Hspec
  import Advent.Day10 (day10a, day10b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day10a" $ do
      it "scores 12" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day10.csv")
        day10a [0..4] csv `shouldBe` 12
      describe "day10b" $ do
        it "day10b" $ do
          day10b ""  `shouldBe` 0
