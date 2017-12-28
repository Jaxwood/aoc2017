module Advent.Day12Spec (spec) where

  import Data.Array
  import Test.Hspec
  import Advent.Day12 (day12a, day12b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day12a" $ do
      it "should find all nodes connected to node 0" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day12.csv")
        day12a csv `shouldBe` (listArray (0,1) [[0],[1]])
