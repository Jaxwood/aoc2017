module Advent.Day15 (day15a, day15b) where

  import Data.Bits

  day15a :: Int -> Int -> Int
  day15a a' b' = length $ filter (uncurry judge) $ take 40000000 $ zip (generatorA (a*a')) (generatorB (b*b'))

  day15b :: Int -> Int -> Int
  day15b a' b' = length $ filter (uncurry judge) $ take 5000000 $ zip (generatorA' (a*a')) (generatorB' (b*b'))

  a :: Int
  a = 16807

  b :: Int
  b = 48271

  divider :: Int
  divider = 2147483647

  generatorA :: Int -> [Int]
  generatorA s = let a' = (a * s) `mod` divider
                 in a':(generatorA a')

  generatorA' :: Int -> [Int]
  generatorA' s = let a' = (a * s) `mod` divider
                  in if a' `mod` 4 == 0 then a':(generatorA' a') else (generatorA' a')

  generatorB :: Int -> [Int]
  generatorB s = let b' = (b * s) `mod` divider
                 in b':(generatorB b')

  generatorB' :: Int -> [Int]
  generatorB' s = let b' = (b * s) `mod` divider
                  in if b' `mod` 8 == 0 then b':(generatorB' b') else (generatorB' b')

  judge :: Int -> Int -> Bool
  judge a' b' = let a'' = (.&.) a' 0xffff
                    b'' = (.&.) b' 0xffff
                in a'' == b''
