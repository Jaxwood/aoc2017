module Advent.Day14 (day14a, day14b) where

  import Advent.Day10 (day10b)
  import Data.Array
  import Data.Char
  import Data.Graph (Graph, dfs)
  import Data.List
  import Text.Printf (printf)

  day14a :: String -> Int
  day14a s = ones $ map binary $ knots [] 0 s

  day14b :: String -> Int
  day14b s = let grid = map binary $ knots [] 0 s
             in length $ uncurry dfs $ graph (pred (128*128)) $ merge $ group' $ sort' $ connect 128 grid

  connect :: Int -> [String] -> [(Int,[Int])]
  connect ln xs = let xsi = map (\(i,x) -> zip [(i*ln)..] x) (zip [0..] xs)
                      xsi' = transpose xsi
                  in concatMap neighbors xsi ++ concatMap neighbors xsi'

  sort' :: [(Int, [Int])] -> [(Int,[Int])]
  sort' = sortBy (\x y -> compare (fst x) (fst y))

  group' :: [(Int, [Int])] -> [[(Int,[Int])]]
  group' = groupBy (\x y -> (fst x) == (fst y))

  merge :: [[(Int, [Int])]] -> [(Int,[Int])]
  merge = map (foldl1 (\(_,acc) (x,acc') -> (x,acc++acc')))

  graph :: Int -> [(Int,[Int])] -> (Graph,[Int])
  graph i is = (array (0,i) is, map fst is)

  neighbors :: [(Int,Char)] -> [(Int,[Int])]
  neighbors [] = []
  neighbors ((x,'1'):y@(y','1'):xs) = (x,[y']):(y',[x]):(neighbors (y:xs))
  neighbors ((x,'1'):xs) = (x,[x]):(neighbors xs)
  neighbors ((x,'0'):xs) = neighbors xs

  ones :: [String] -> Int
  ones = sum . map (length . filter (=='1'))

  knots :: [String] -> Int -> String -> [String]
  knots acc 128 s = acc
  knots acc c s = let r = day10b [0..255] (printf "%s-%d" s c)
                      c' = succ c
                  in knots (r:acc) c' s

  binary :: String -> String
  binary xs = concat $ map (printf "%0.4b") $ map digitToInt xs
