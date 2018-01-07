module Advent.Day19Spec (spec) where

  import Test.Hspec
  import System.Directory
  import Data.Map
  import Advent.Day19 (day19a, day19b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day19a" $ do
      it "should find path through maze" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day19.csv")
        day19a csv `shouldBe` "ABCDEF"
