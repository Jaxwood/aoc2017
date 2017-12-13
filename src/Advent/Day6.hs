module Advent.Day6 (day6a, day6b) where

  day6a :: String -> Int
  day6a a = sum $ map read $ words a

  day6b :: String -> Int
  day6b a = 0
