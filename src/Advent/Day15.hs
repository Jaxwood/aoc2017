module Advent.Day15 (day15a, day15b) where

  import Data.Bits

  day15a :: Int -> Int -> Int
  day15a a' b' = length $ filter (uncurry judge) $ take 40000000 $ zip (generatorA (a*a')) (generatorB (b*b'))

  day15b :: String -> Int
  day15b s = 0

  a :: Int
  a = 16807

  b :: Int
  b = 48271

  divider :: Int
  divider = 2147483647

  generatorA :: Int -> [Int]
  generatorA s = let a' = (a * s) `mod` divider
                 in a':(generatorA a')

  generatorB :: Int -> [Int]
  generatorB s = let b' = (b * s) `mod` divider
                 in b':(generatorB b')

  judge :: Int -> Int -> Bool
  judge a' b' = let a'' = (.&.) a' 0xffff
                    b'' = (.&.) b' 0xffff
                in a'' == b''
