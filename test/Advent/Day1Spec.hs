module Advent.Day1Spec (spec) where
  import Test.Hspec
  import Advent.Day1 (day1a)
  import System.Directory

  main :: IO ()
  main = do
    hspec spec

  spec :: Spec
  spec = do
    describe "day1a" $ do
      it "first and last digit matches" $ do
        day1a "1122" `shouldBe` 3
      it "all digit matches" $ do
        day1a "1111" `shouldBe` 4
      it "no digit matches" $ do
        day1a "1234" `shouldBe` 0
      it "only last digit matches" $ do
        day1a "91212129" `shouldBe` 9
      it "solution" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day1.csv")
        day1a csv `shouldBe` 0
