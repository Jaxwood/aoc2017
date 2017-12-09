module Advent.Day5 (day5a, day5b) where

  import Data.List

  day5a :: String -> Int
  day5a = jumps 0 0 . map read . lines

  day5b :: String -> Int
  day5b = jumps' 0 0 . map read . lines

  jumps :: Int -> Int -> [Int] -> Int
  jumps idx acc xs = if length xs <= idx
                     then 
                       acc
                     else
                       let x = xs !! idx
                           idx' = if x == 0 then idx else idx + x
                           (f,(l:ls)) = splitAt idx xs
                       in jumps idx' (succ acc) $ f ++ (succ l):ls

  jumps' :: Int -> Int -> [Int] -> Int
  jumps' idx acc xs = if length xs <= idx
                      then 
                        acc
                      else
                        let x = xs !! idx
                            idx' = if x == 0 then idx else idx + x
                            (f,(l:ls)) = splitAt idx xs
                        in jumps' idx' (succ acc) $ f ++ (if x > 2 then pred l else succ l):ls
