module Advent.Day3 (day3a, day3b) where

  import Data.List

  day3a :: Int -> Int
  day3a 1 = 0
  day3a n = toValue $ head $ filter (port n) $ concatMap generate $ filter (inBounds n) $ map bounds [1..]

  day3b :: Int -> Int
  day3b n = toValue' $ head . dropWhile (\(x,_) -> x <= n) $ foldl findNeighbors [] coords

  toValue :: (Int,(Int,Int)) -> Int
  toValue (_, (l,h)) = abs l + abs h

  toValue' :: (Int,(Int,Int)) -> Int
  toValue'(n, _) = n

  inBounds :: Int -> (Int, Int, Int) -> Bool
  inBounds n (l,h,_) = n > l && n < h

  port :: Int -> (Int, (Int, Int)) -> Bool
  port n (x,_) = x == n

  coords :: [(Int,(Int,Int))]
  coords = (1, (0,0)):concatMap (generate . bounds) [1..50]

  findNeighbors :: [(Int, (Int,Int))] -> (Int,(Int,Int)) -> [(Int, (Int,Int))]
  findNeighbors acc h@(1,(0,0)) = acc ++ [h]
  findNeighbors acc (_,c) = let n' = sumCells c acc
                            in acc ++ [(n',c)]

  sumCells :: (Int,Int) -> [(Int, (Int,Int))] -> Int
  sumCells c cs = foldr (\(n,_) acc -> acc + n) 0 $ filter (\(n, c') -> c' `elem` (cells c)) cs

  cells :: (Int,Int) -> [(Int,Int)]
  cells (x,y) = [(x',y')|x' <- [pred x..succ x], y' <- [pred y..succ y], (x,y) /= (x',y')]

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
