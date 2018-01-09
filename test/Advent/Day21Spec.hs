module Advent.Day21Spec (spec) where

  import Test.Hspec
  import System.Directory
  import Advent.Day21 (day21a, day21b, flip', flip'')


  prettyPrint :: [[Char]] -> [IO ()]
  prettyPrint iss = do
    fmap putStrLn iss

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day21a" $ do
      it "find pixel on after 2 iterations" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day21.csv")
        day21a csv `shouldBe` 12
      it "flips top and bottom" $ do
        flip' [".#.","..#","###"] `shouldBe` ["###","..#", ".#."]
      it "flips top and bottom evenly" $ do
        flip' [".#..", "...#", "#..#", "####"] `shouldBe` ["####", "#..#", "...#", ".#.."]
      it "flips left and right" $ do
        flip'' [".#.","..#","###"] `shouldBe` [".#.","#..","###"]
      it "flips left and right evenly" $ do
        flip'' [".#..", "...#", "#..#", "####"] `shouldBe` ["..#.", "#...", "#..#", "####"]
