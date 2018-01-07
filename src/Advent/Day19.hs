module Advent.Day19 (day19a, day19b) where

  data Token = Pipe Int | Cross Int | Dash Int | Letter Int Char deriving (Show,Eq)

  day19a :: String -> String
  day19a s = let a = map (tokens . indexes) $ lines s
             in ""

  day19b :: String -> Int
  day19b s = 0

  indexes :: String -> [(Int,Char)]
  indexes = zip [0..]

  tokens :: [(Int,Char)] -> [Token]
  tokens [] = []
  tokens ((i,'|'):ts) = (Pipe i):tokens ts
  tokens ((i,'+'):ts) = (Cross i):tokens ts
  tokens ((i,'-'):ts) = (Dash i):tokens ts
  tokens ((i,' '):ts) = tokens ts
  tokens ((i,t):ts) = (Letter i t):tokens ts

