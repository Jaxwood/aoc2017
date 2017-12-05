module Advent.Day3 (day3a, day3b) where

  day3a :: Int -> Int
  day3a a = (\(a,b) -> (abs a) + (abs b)) $ coords !! a

  day3b :: Int -> Int
  day3b = id

  addX :: (Int,Int) -> (Int,Int)
  addX (a,b) = (succ a,b)

  addY :: (Int,Int) -> (Int,Int)
  addY (a,b) = (a,succ b)

  decX :: (Int,Int) -> (Int,Int)
  decX (a,b) = (pred a,b)

  decY :: (Int,Int) -> (Int,Int)
  decY (a,b) = (a,pred b)

  operations :: [(Int,Int) -> (Int,Int)]
  operations = cycle [addY,decX,decY,addX]

  intList :: Int -> [[Int]]
  intList n =  concat [take 2 $ repeat $ replicate x x | x <- [2..n]]

  mm :: [([Int], (Int, Int) -> (Int,Int))]
  mm = zip (intList 1024) operations

  coords :: [(Int,Int)]
  coords = (0,0):(0,1):(foldl (\acc (n,f) -> (init acc) ++ foldl (\acc' _ -> acc' ++ [f $ last acc']) [last acc] n) [(1,1)] mm)
