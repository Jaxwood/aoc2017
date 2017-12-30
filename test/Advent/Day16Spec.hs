module Advent.Day16Spec (spec) where

  import Test.Hspec
  import Advent.Day16 (day16a, day16b)

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day16a" $ do
      it "should find order after dance" $ do
        day16a "abcde" `shouldBe` "baedc"
    describe "day16b" $ do
      it "-" $ do
        day16b "" `shouldBe` ""
