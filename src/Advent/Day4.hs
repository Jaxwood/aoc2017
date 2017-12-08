module Advent.Day4 (day4a, day4b) where

  import Data.List

  day4a :: String -> Int
  day4a = length . filter (\x -> x == False) . map (any (>1) . map length . group . sort . words) . lines

  day4b :: String -> Int
  day4b = length . filter (\x -> x == False) . map (any (>1) . map length . group . map sort . sort . words) . lines


