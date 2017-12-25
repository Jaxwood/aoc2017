module Advent.Day9Spec (spec) where

  import Test.Hspec
  import Advent.Day9 (day9a, day9b)
  import Data.Map
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day9a" $ do
      it "{} scores 1" $ do
        day9a "{}"  `shouldBe` 1
      it "{{{}}} scores 6" $ do
        day9a "{{{}}}"  `shouldBe` 6
      it "{{},{}} scores 5" $ do
        day9a "{{},{}}"  `shouldBe` 5
      it "{{{},{},{{}}}} scores 16" $ do
        day9a "{{{},{},{{}}}}"  `shouldBe` 16
      -- it "{<a>,<a>,<a>,<a>} scores 1" $ do
      --  day9a "{<a>,<a>,<a>,<a>}"  `shouldBe` 1
      --it "{{<ab>},{<ab>},{<ab>},{<ab>}} scores 9" $ do
--        day9a "{{<ab>},{<ab>},{<ab>},{<ab>}}"  `shouldBe` 9
 --     it "{{<!!>},{<!!>},{<!!>},{<!!>}} scores 9" $ do
  --      day9a "{{<!!>},{<!!>},{<!!>},{<!!>}}"  `shouldBe` 9
   --   it "{{<a!>},{<a!>},{<a!>},{<ab>}} scores 3" $ do
    --    day9a "{{<a!>},{<a!>},{<a!>},{<ab>}}"  `shouldBe` 3
    describe "day9b" $ do
      it "reach exit" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day9.csv")
        day9b csv  `shouldBe` 0
