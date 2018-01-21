module Advent.Day23Spec (spec) where

  import qualified Data.Map as M
  import Test.Hspec
  import System.Directory
  import Advent.Day23 (day23a, day23b)

  main :: IO ()
  main = do
    hspec spec
  
  spec :: Spec
  spec = do
    describe "day23a" $ do
     it "should calculate times mul is called" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day23.csv")
        day23a csv `shouldBe` 9409
    describe "day23b" $ do
      it "should find value of register h" $ do
        day23b `shouldBe` 913
