module Advent.Day3 (day3a, day3b) where

  import Data.List

  day3a :: Int -> Int
  day3a = sumPairs . toPairs

  day3b :: Int -> Int
  day3b = id

  toPairs :: Int -> (Int, (Int, Int))
  toPairs 1 = (1, (0,0))
  toPairs a = let c@(l,h) = range a
              in head $ filter (\(f,s) -> f == a) $ zip [l..h] $ coords c (rangeIdx c)

  sumPairs :: (Int, (Int,Int)) -> Int
  sumPairs (_,(a,b)) = abs a + abs b

  range :: Int -> (Int,Int)
  range a = head $ filter (\(l,h) -> l < a && a < h) ranges

  ranges :: [(Int, Int)]
  ranges = map (\x -> ((x-2) * (x-2) + 1,x * x)) [3,5..]

  rangeIdx :: (Int,Int) -> Int
  rangeIdx n = case findIndex ((==)n) ranges of
               Just x -> succ x
               Nothing -> 0

  coords :: (Int,Int) -> Int -> [(Int,Int)]
  coords (l,h) n = foldl (\acc (ls,f) -> (init acc) ++ foldl (\acc' _ -> acc' ++ [f $ last acc']) [last acc] ls) [first] $ zip [[l..tr'], [tr..tl'], [tl..bl'], [bl..br]] ops 
                   where n' = n+n
                         n'' = -n
                         first = (n,succ n'')
                         br = pred h
                         bl = h-n'
                         bl' = pred bl
                         tl = bl - n'
                         tl' = pred tl
                         tr = tl - n'
                         tr' = pred tr

  ops :: [(Int,Int) -> (Int,Int)]
  ops = [addY, decX, decY, addX]

  addX :: (Int,Int) -> (Int,Int)
  addX (a,b) = (succ a,b)

  decX :: (Int,Int) -> (Int,Int)
  decX (a,b) = (pred a,b)

  addY :: (Int,Int) -> (Int,Int)
  addY (a,b) = (a,succ b)

  decY :: (Int,Int) -> (Int,Int)
  decY (a,b) = (a,pred b)
