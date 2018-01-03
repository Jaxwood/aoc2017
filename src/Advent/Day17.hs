module Advent.Day17 (day17a, day17b) where

    import qualified Data.Sequence as Seq

    day17a :: Int -> Int
    day17a s = spinlock 1 s 0 (Seq.singleton 0)

    day17b :: Int -> Int
    day17b s = spinlock' 1 s 0 (Seq.singleton 0)

    spinlock :: Int -> Int -> Int -> Seq.Seq Int -> Int
    spinlock 2018 _ _ is = let (Just idx) = Seq.elemIndexL 2017 is
                           in Seq.index is (succ idx)
    spinlock num size idx is = let idx' = (idx + size) `mod` (Seq.length is)
                                   idx'' = succ idx'
                               in spinlock (succ num) size idx'' (Seq.insertAt idx'' num is)

    spinlock' :: Int -> Int -> Int -> Seq.Seq Int -> Int
    spinlock' 50000001 _ _ is = Seq.index is 1
    spinlock' num size idx is = let idx' = (idx + size) `mod` (Seq.length is)
                                    idx'' = succ idx'
                                in spinlock' (succ num) size idx'' (Seq.insertAt idx'' num is)
