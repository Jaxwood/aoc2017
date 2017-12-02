module Advent.Day1 (day1a, day1b) where

  import Data.Char

  day1a :: String -> Int
  day1a s = sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ zip s s'
            where s' = tail s ++ [head s]

  day1b :: String -> Int
  day1b s = sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ zip s s'
            where half = length s `div` 2
                  s' = drop half s ++ take half s
