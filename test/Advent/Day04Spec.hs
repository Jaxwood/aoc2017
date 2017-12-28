module Advent.Day04Spec (spec) where

  import Test.Hspec
  import Advent.Day04 (day4a, day4b)
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
    describe "day4b" $ do
      it "no duplicate" $ do
        day4b "abcde fghij" `shouldBe` 1
      it "duplicate anagram" $ do
        day4b "abcde xyz ecdab" `shouldBe` 0
      it "all letter should be used for anagram" $ do
        day4b "a ab abc abd abf abj" `shouldBe` 1
      it "valid word" $ do
        day4b "iiii oiii ooii oooi oooo" `shouldBe` 1
      it "invalid word" $ do
        day4b "oiii ioii iioi iiio" `shouldBe` 0
