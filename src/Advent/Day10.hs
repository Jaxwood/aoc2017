module Advent.Day10 (day10a, day10b) where

  import Data.List
  import Text.Printf
  import Data.Bits (xor)
  import Data.Char (ord)
  import qualified Data.Text as T

  day10a :: [Int] -> String -> Int
  day10a xs ys = let ys' = map (read . T.unpack) $ T.split (==',') $ T.pack ys
                     (xs', _) = hash (0,0) xs ys'
                 in check xs'

  day10b :: [Int] -> String -> String
  day10b xs s = asHex $ sparse $ hash' 64 (0,0) xs $ lengthSequence s

  check :: [Int] -> Int
  check (x:y:xs) = x * y

  hash' :: Int -> (Int,Int) -> [Int] -> [Int] -> [Int]
  hash' 0 _ xs _ = xs
  hash' rounds s xs ys = let (xs',s') = hash s xs ys
                         in hash' (pred rounds) s' xs' ys

  hash :: (Int,Int) -> [Int] -> [Int] -> ([Int], (Int,Int))
  hash s xs [] = (xs,s)
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
  position len amnt idx skp = if a <= 0 then b - len else b
                              where a = len - b
                                    b = idx + amnt + skp

  rotate :: Int -> [a] -> [a]
  rotate _ [] = []
  rotate n xs = zipWith const (drop n (cycle xs)) xs

  lengthSequence :: String -> [Int]
  lengthSequence s = (map ord s) ++ [17, 31, 73, 47, 23]

  sparse :: [Int] -> [Int]
  sparse = map (foldl1 xor) . sixteens

  sixteens :: [Int] -> [[Int]]
  sixteens [] = []
  sixteens xs = (take 16 xs):(sixteens $ drop 16 xs)

  asHex :: [Int] -> String
  asHex = foldl (++) "" . map (\x -> if x < 16 then printf "0%x" x else printf "%x" x)
