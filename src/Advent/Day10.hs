module Advent.Day10 (day10a, day10b) where

  import Data.List

  day10a :: [Int] -> [Int] -> Int
  day10a xs ys = check $ hash 0 xs ys

  day10b :: String -> Int
  day10b s = 0

  check :: [Int] -> Int
  check (x:y:xs) = x * y

  hash :: Int -> [Int] -> [Int] -> [Int]
  hash s xs [] = xs
  hash s xs (y:ys) = let xs' = rmi s y xs
                     in hash (succ s) xs' ys

  rmi :: Int -> Int -> [Int] -> [Int]
  rmi s y xs = let xs' = cycle xs
                   xs'' = reverse $ take y $ drop s xs' 
                   xs''' = drop y xs'
               in xs

