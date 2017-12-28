module Advent.Day01Spec (spec) where
  import Test.Hspec
  import Advent.Day01 (day1a, day1b)
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
      it "with input" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day1.csv")
        day1a csv `shouldBe` 1223
    describe "day1b" $ do
      describe "all digit matches" $ do
        it "1212" $ do
          day1b "1212" `shouldBe` 6
        it "123123" $ do
          day1b "123123" `shouldBe` 12
      it "no digit match" $ do
        day1b "1221" `shouldBe` 0
      it "one digit match" $ do
        day1b "123425" `shouldBe` 4
      it "no digit pattern" $ do
        day1b "12131415" `shouldBe` 4
      it "with input" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day1.csv")
        day1b csv `shouldBe` 1284
