module Advent.Day10Spec (spec) where

  import Test.Hspec
  import Advent.Day10 (day10a, day10b, lengthSequence, sparseHash, sparse)
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
      describe "toAscii" $ do
        it "coverts to ascii" $ do
          lengthSequence "1,2,3"  `shouldBe` [49,44,50,44,51,17,31,73,47,23]
      describe "sparse hash" $ do
        it "xor 16 numbers" $ do
          sparseHash [65,27,9,1,4,3,40,50,91,7,6,0,2,5,68,22]  `shouldBe` 64
      describe "sparse" $ do
        it "xor list of numbers" $ do
          sparse [65,27,9,1,4,3,40,50,91,7,6,0,2,5,68,22,65,27,9,1,4,3,40,50,91,7,6,0,2,5,68,22]  `shouldBe` [64,64]
