module Advent.Day25Spec (spec) where

  import Test.Hspec
  import qualified Data.Vector as V
  import System.Directory
  import Advent.Day25 (day25a, day25b)

  main :: IO ()
  main = do
    hspec spec
  
  spec :: Spec
  spec = do
    describe "day25a" $ do
     it "should calculate the diagnostic checksum after 6 steps" $ do
        dir <- getCurrentDirectory
        csv <- readFile (dir ++ "/test/day25.csv")
        day25a csv `shouldBe` 3
