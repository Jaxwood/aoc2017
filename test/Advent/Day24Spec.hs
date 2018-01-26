module Advent.Day24Spec (spec) where

  import Test.Hspec
  import System.Directory
  import Advent.Day24 (day24a, day24b)

  main :: IO ()
  main = do
    hspec spec
  
  spec :: Spec
  spec = do
    describe "day24a" $ do
     it "should find the strongest bridge" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day24.csv")
        day24a csv `shouldBe` 31
    describe "day24a" $ do
     it "should find the longest bridge" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day24.csv")
        day24b csv `shouldBe` 19
