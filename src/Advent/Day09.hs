module Advent.Day09 (day9a, day9b) where

  day9a :: String -> Int
  day9a = parse 0

  day9b :: String -> Int
  day9b = parse' 0

  parse :: Int -> String -> Int
  parse acc [] = acc
  parse acc ('{':xs) = let (xs',acc') = parseEnd 1 (succ acc) xs
                       in parse acc' xs'
  parse acc (x:xs) = parse acc xs

  parseEnd :: Int -> Int -> String -> (String, Int)
  parseEnd acc' acc ('<':xs) = parseEnd acc' acc (parseGarbage xs)
  parseEnd acc' acc ('{':xs) = parseEnd (succ acc') (acc + succ acc') xs
  parseEnd acc' acc ('}':xs) = if acc' == 1 then (xs, acc) else parseEnd (pred acc') acc xs
  parseEnd acc' acc (x:xs) = parseEnd acc' acc xs

  parseGarbage :: String -> String
  parseGarbage ('!':'!':xs) = parseGarbage xs
  parseGarbage ('!':'>':xs) = parseGarbage xs
  parseGarbage ('>':xs) = xs
  parseGarbage (x:xs) = parseGarbage xs

  parse' :: Int -> String -> Int
  parse' acc [] = acc
  parse' acc ('<':xs) = let (xs', acc') = countGarbage acc xs
                        in parse' acc' xs'
  parse' acc (x:xs) = parse' acc xs

  countGarbage :: Int -> String -> (String, Int)
  countGarbage acc [] = ([], acc)
  countGarbage acc ('!':x:xs) = countGarbage acc xs
  countGarbage acc ('<':xs) = countGarbage (succ acc) xs
  countGarbage acc ('>':xs) = (xs, acc)
  countGarbage acc (x:xs) = countGarbage (succ acc) xs