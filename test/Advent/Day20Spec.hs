module Advent.Day20Spec (spec) where

  import Test.Hspec
  import System.Directory
  import Advent.Day20 (day20a, day20b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day20a" $ do
      it "should find particle closest to <0,0,0>" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day20.csv")
        day20a csv `shouldBe` []
