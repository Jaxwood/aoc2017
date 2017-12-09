module Advent.Day3 (day3a, day3b) where

  import Data.List

  day3a :: Int -> Int
  day3a 1 = 0
  day3a n = getValue n $ generate $ head $ filter (\(l,h,_) -> n > l && n < h) $ map bounds [1..]

  day3b :: Int -> Int
  day3b = id

  getValue :: Int -> [(Int, (Int, Int))] -> Int
  getValue n xs = let (_, (f,l)) = head $ filter (\(x, _) -> x == n) xs
                  in abs f + abs l

  generate :: (Int,Int,Int) -> [(Int, (Int,Int))]
  generate (f,l,n) = let right = [(n,x) |x <- [-n+1..n]]
                         top = [(x,n) |x <- reverse [-n..n-1]]
                         left = [(-n,x) |x <- reverse [-n..n-1]]
                         bottom = [(x,-n) |x <- [-n+1..n]]
                     in zip [f..l] $ right ++ top ++ left ++ bottom

  interval :: [Int]
  interval = [3,5..]

  bounds :: Int -> (Int,Int,Int)
  bounds 1 = (2,9,1)
  bounds n = let high = interval !! (n - 1)
                 low = interval !! (n - 2)
             in (low * low + 1, high * high, n)