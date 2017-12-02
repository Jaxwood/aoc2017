module Advent.Day2 (day2a, day2b, difference, diviable) where

  import Data.List

  day2a :: String -> Int
  day2a = sum . map difference . lines

  day2b :: String -> Int
  day2b = sum . map (diviable . map read . words) . lines

  difference :: String -> Int
  difference s = max'' - min''
                 where s' = map read $ words s
                       max'' = maximum s'
                       min'' = minimum s'

  diviable :: [Int] -> Int
  diviable (x:xs) = if any (\x' -> x' `mod` x == 0) xs then sum $ map (\x' -> x' `div` x) xs else diviable xs
  diviable [] = 0
    