module Advent.Day21 (day21a, day21b) where

  import Data.List

  day21a :: String -> Int
  day21a s = 0

  day21b :: String -> Int
  day21b s = 0

  pattern :: [[Char]] -> [[[Char]]]
  pattern iss = (flip' iss):(flip'' iss):(rotate iss)

  rotate :: [[Char]] -> [[[Char]]]
  rotate iss = take 4 $ iterate (map reverse . transpose) iss

  flip' :: [[Char]] -> [[Char]]
  flip' = reverse

  flip'' :: [[Char]] -> [[Char]]
  flip'' = map reverse

  size :: [[Char]] -> Int
  size = length

  on :: [[Char]] -> Int
  on = length . filter (=='#') . concat
