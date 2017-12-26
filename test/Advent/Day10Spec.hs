module Advent.Day10Spec (spec) where

  import Test.Hspec
  import Advent.Day10 (day10a, day10b)
  import Data.Map
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day10a" $ do
      it "{} scores 12" $ do
        day10a [0..4] [3,4,1,5]  `shouldBe` 12
    describe "day10b" $ do
      it "{} scores 1" $ do
        day10b "{}"  `shouldBe` 0
