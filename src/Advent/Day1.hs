module Advent.Day1 (day1a) where

  import Data.Char

  day1a :: String -> Int
  day1a s = sum $ map (\(a,_) -> digitToInt a) $ filter (\a -> fst a == snd a) $ zip s ((tail s) ++ ([head s]))
