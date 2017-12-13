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
  redistribute xs m (Nothing) = xs
  redistribute xs m (Just idx) =
    let ln = pred $ length xs
        am = div m ln
        r = m `mod` ln
    in map (\(i,v) -> if i == idx then r else v + am) $  zip [0..] xs
