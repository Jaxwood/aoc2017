module Advent.Day10 (day10a, day10b, lengthSequence, sparseHash) where

  import Data.List
  import Data.Bits (xor)
  import Data.Char (ord)
  import qualified Data.Text as T

  day10a :: [Int] -> String -> Int
  day10a xs ys = let ys' = map (read . T.unpack) $ T.split (==',') $ T.pack ys
                 in check $ hash (0,0) xs ys'

  day10b :: String -> Int
  day10b s = 0

  check :: [Int] -> Int
  check (x:y:xs) = x * y

  hash :: (Int,Int) -> [Int] -> [Int] -> [Int]
  hash s xs [] = xs
  hash (s,idx) xs (y:ys) = let ys' = reverse' idx y xs
                               xl = length xs
                               idx' = position xl y idx s
                               xs' = stitch idx xs ys' 
                           in hash (succ s, idx') xs' ys

  stitch :: Int -> [Int] -> [Int] -> [Int]
  stitch idx xs ys = rotate (xl-idx) $ take xl $ ys ++ drop yl ys'
                     where xl = length xs
                           yl = length ys
                           ys' = rotate idx $ cycle xs

  reverse' :: Int -> Int -> [Int] -> [Int]
  reverse' idx y xs = reverse $ take y $ drop idx $ cycle xs 

  position :: Int -> Int -> Int -> Int -> Int
  position len amnt idx skp = if a <= 0 then (idx + amnt + skp) - len else b
                              where a = len - b
                                    b = idx + amnt + skp

  rotate :: Int -> [a] -> [a]
  rotate _ [] = []
  rotate n xs = zipWith const (drop n (cycle xs)) xs

  lengthSequence :: String -> [Int]
  lengthSequence s = (map ord s) ++ [17, 31, 73, 47, 23]

  sparseHash :: [Int] -> Int
  sparseHash = foldl xor 0
