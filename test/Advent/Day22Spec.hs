module Advent.Day22Spec (spec) where

  import qualified Data.Map as M
  import Test.Hspec
  import System.Directory
  import Advent.Day22 (day22a, day22b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day22a" $ do
      it "should count infections after 10000 burst of activity" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day22.csv")
        day22a csv `shouldBe` 5587
    describe "day22b" $ do
      it "should count infections after 100 burst of activity" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day22.csv")
        day22b csv `shouldBe` 26
