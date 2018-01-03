module Advent.Day17 (day17a, day17b) where

    import Data.List

    day17a :: Int -> Int
    day17a s = spinlock 1 s 0 [0]

    day17b :: Int -> Int
    day17b i = i

    spinlock :: Int -> Int -> Int -> [Int] -> Int
    spinlock 2018 _ _ is = let (Just a) = elemIndex 2017 is
                               (hd,ls) = splitAt (succ a) (cycle is)
                           in head ls
    spinlock num size idx is = let a = (drop idx $ cycle is) !! size
                                   (Just b) = elemIndex a is
                                   idx' = succ b
                                   (hd,ls) = splitAt idx' is
                               in spinlock (succ num) size idx' (hd ++ (num:ls))
