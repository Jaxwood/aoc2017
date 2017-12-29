module Advent.Day14 (day14a, day14b) where

  import Advent.Day10 (day10b)
  import Data.Char
  import Text.Printf (printf)

  day14a :: String -> Int
  day14a s = ones $ map binary $ knots [] 0 s

  day14b :: String -> Int
  day14b s = 0

  ones :: [String] -> Int
  ones = sum . map (length . filter (=='1'))

  knots :: [String] -> Int -> String -> [String]
  knots acc 128 s = acc
  knots acc c s = let r = day10b [0..255] (printf "%s-%d" s c)
                      c' = succ c
                  in knots (r:acc) c' s

  binary :: String -> String
  binary xs = concat $ map (printf "%0.4b") $ map digitToInt xs
