module Advent.Day2 (day2a, day2b, difference) where

  day2a :: String -> Int
  day2a s = 0

  day2b :: String -> Int
  day2b s = 0

  difference :: String -> Int
  difference s = max'' - min''
                 where s' = map read $ words s
                       max'' = maximum s'
                       min'' = minimum s'
