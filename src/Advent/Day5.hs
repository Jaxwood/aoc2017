{-# LANGUAGE ViewPatterns #-}
module Advent.Day5 (day5a, day5b) where

  import Data.Sequence as S

  day5a :: String -> Int
  day5a = jumps 0 0 . fromList . map read . lines

  day5b :: String -> Int
  day5b = jumps' 0 0 . fromList . map read . lines

  jumps :: Int -> Int -> Seq Int -> Int
  jumps idx acc xs = if S.length xs <= idx
                     then 
                       acc
                     else
                       let x = index xs idx
                           idx' = if x == 0 then idx else idx + x
                           (f,(S.viewl -> l:<ls)) = S.splitAt idx xs
                       in jumps idx' (succ acc) $ f >< (succ l) <| ls

  jumps' :: Int -> Int -> Seq Int -> Int
  jumps' idx acc xs =  if S.length xs <= idx
                       then 
                         acc
                       else
                         let x = index xs idx
                             idx' = if x == 0 then idx else idx + x
                             (f,(S.viewl -> l:<ls)) = S.splitAt idx xs
                         in jumps' idx' (succ acc) $ f >< (if l > 2 then pred l else succ l) <| ls
