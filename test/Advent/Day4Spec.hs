module Advent.Day4Spec (spec) where

  import Test.Hspec
  import Advent.Day4 (day4a, day4b)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day4a" $ do
      it "no duplicate" $ do
        day4a "aa bb cc dd ee" `shouldBe` 1
      it "duplicate" $ do
        day4a "aa bb cc dd ee aa" `shouldBe` 0
      it "duplicate but different length" $ do
        day4a "aa bb cc dd ee aaa" `shouldBe` 1
