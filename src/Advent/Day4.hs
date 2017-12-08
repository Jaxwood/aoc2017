module Advent.Day4 (day4a, day4b) where

  import Data.List

  day4a :: String -> Bool
  day4a = all (==1) . map length . group . sort . words

  day4b :: Int -> Int
  day4b = id

