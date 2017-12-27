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
        it "dense hash with empty input" $ do
          day10b [0..255] "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
        it "dense hash with input AoC 2017" $ do
          day10b [0..255] "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
        it "dense hash with input 1,2,3" $ do
          day10b [0..255] "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
        it "dense hash with input 1,2,4" $ do
          day10b [0..255] "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
