module Advent.Day8Spec (spec) where

  import Test.Hspec
  import Advent.Day8 (day8a, day8b)
  import Data.Map
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day8a" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day8.csv")
        day8a csv  `shouldBe` empty
    describe "day8b" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day8.csv")
        day8b csv  `shouldBe` 0
