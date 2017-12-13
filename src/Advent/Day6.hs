module Advent.Day6 (day6a, day6b) where

  import Data.List

  day6a :: String -> Int
  day6a a = findMax [] $ map read $ words a

  day6b :: String -> Int
  day6b a = 0

  findMax :: [[Int]] -> [Int] -> Int
  findMax acc xs
    | xs `elem` acc = length acc
    | otherwise = let m = maximum xs
                      idx = findIndex ((==)m) xs
                      candidate = redistribute xs m idx
                  in findMax (xs:acc) candidate

  redistribute :: [Int] -> Int -> Maybe Int -> [Int]
  redistribute xs _ (Nothing) = xs
  redistribute xs mx (Just idx)
    | mx < ln = let a = take ln $ 0:(distribute xs' mx)
                in take ln $ drop (ln - idx) $ cycle a
    | otherwise = map (\(idx',val) -> if idx' == idx then remainder else val+quotient) $ zip [0..] xs
    where ln = length xs
          ln' = pred ln
          remainder = mx `mod` ln'
          quotient = mx `div` ln'
          xs' = drop (succ idx) $ cycle xs

  distribute :: [Int] -> Int -> [Int]
  distribute xs 0 = xs
  distribute [] _ = []
  distribute (x:xs) v = (succ x):(distribute xs $ pred v)
    
