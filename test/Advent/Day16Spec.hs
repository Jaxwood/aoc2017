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
        day16a csv ['a'..'e'] `shouldBe` "baedc"
    describe "day16b" $ do
      it "should find order 1 billion dances" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day16.csv")
        day16b csv ['a'..'e'] 2 `shouldBe` "ceadb"
