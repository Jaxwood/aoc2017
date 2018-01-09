module Advent.Day21 (day21a, day21b, flip', flip'') where

  import Data.List

  day21a :: String -> Int
  day21a s = 0

  day21b :: String -> Int
  day21b s = 0

  rotate :: [[Char]] -> [[Char]]
  rotate iss = iss

  flip' :: [[Char]] -> [[Char]]
  flip' = reverse

  flip'' :: [[Char]] -> [[Char]]
  flip'' = map reverse

  size :: [[Char]] -> Int
  size = length

  on :: [[Char]] -> Int
  on = length . filter (=='#') . concat
