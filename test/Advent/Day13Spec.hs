module Advent.Day13Spec (spec) where

  import Test.Hspec
  import Advent.Day13 (day13a, day13b, Layer(Layer))
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day13a" $ do
      it "should find severity" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day13.csv")
        day13a csv `shouldBe` []
    describe "day13b" $ do
      it "should find severity" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day13.csv")
        day13b csv `shouldBe` 0
