module Advent.Day16Spec (spec) where

  import Test.Hspec
  import Advent.Day16 (day16a, day16b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day16a" $ do
      it "should find order after dance" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day16.csv")
        day16a "abcde" csv `shouldBe` []
    describe "day16b" $ do
      it "-" $ do
        day16b "" `shouldBe` ""
