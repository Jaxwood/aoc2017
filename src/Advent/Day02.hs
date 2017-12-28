module Advent.Day02 (day2a, day2b) where

  import Data.List

  day2a :: String -> Int
  day2a = sum . map difference . lines

  day2b :: String -> Int
  day2b = sum . map (dividable . sort . map read . words) . lines

  difference :: String -> Int
  difference s = max'' - min''
                 where s' = map read $ words s
                       max'' = maximum s'
                       min'' = minimum s'

  dividable :: [Int] -> Int
  dividable (x:xs) = if null xs' then dividable xs else sum $ map (\x' -> x' `div` x) xs' 
                      where xs' = filter (\x' -> x' `mod` x == 0) xs
  dividable [] = 0
    
