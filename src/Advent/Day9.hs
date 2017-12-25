module Advent.Day9 (day9a, day9b) where

  day9a :: String -> Int
  day9a = parse 0

  day9b :: String -> Int
  day9b s = 0

  parse :: Int -> String -> Int
  parse acc [] = acc
  parse acc ('{':xs) = parseEnd acc xs
  parse acc (x:xs) = parse acc xs

  parseEnd :: Int -> String -> Int
  parseEnd acc ('{':xs) = parseEnd (succ acc) xs
  parseEnd acc ('}':xs) = parse (succ acc) xs
  parseEnd acc (x:xs) = parseEnd acc xs

