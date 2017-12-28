module Advent.Day09Spec (spec) where

  import Test.Hspec
  import Advent.Day09 (day9a, day9b)
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
      it "{<a>,<a>,<a>,<a>} scores 1" $ do
        day9a "{<a>,<a>,<a>,<a>}"  `shouldBe` 1
      it "{{<ab>},{<ab>},{<ab>},{<ab>}} scores 9" $ do
        day9a "{{<ab>},{<ab>},{<ab>},{<ab>}}"  `shouldBe` 9
      it "{{<!!>},{<!!>},{<!!>},{<!!>}} scores 9" $ do
        day9a "{{<!!>},{<!!>},{<!!>},{<!!>}}"  `shouldBe` 9
      it "{{<a!>},{<a!>},{<a!>},{<ab>}} scores 3" $ do
        day9a "{{<a!>},{<a!>},{<a!>},{<ab>}}"  `shouldBe` 3
      it "{{<!>},{<!>},{<!>},{<a>}} scores 2" $ do
        day9a "{{<!>},{<!>},{<!>},{<a>}}" `shouldBe` 3
    describe "day9b" $ do
      it "<> scores 0" $ do
        day9b "<>"  `shouldBe` 0
      it "<random characters> scores 17" $ do
        day9b "<random characters>"  `shouldBe` 17
      it "<<<<> scores 3" $ do
        day9b "<<<<>"  `shouldBe` 3
      it "<{!>}> scores 2" $ do
        day9b "<{!>}>"  `shouldBe` 2
      it "<!!> scores 0" $ do
        day9b "<!!>"  `shouldBe` 0
      it "<!!!>> scores 0" $ do
        day9b "<!!!>>"  `shouldBe` 0
      it "<{o\"i!a,<{i<a> scores 10" $ do
        day9b "<{o\"i!a,<{i<a>"  `shouldBe` 10
