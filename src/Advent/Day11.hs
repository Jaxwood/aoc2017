module Advent.Day11 (day11a, day11b) where

  import qualified Data.Text as T

  data Cardinal = N | NE | E | SE | S | SW | W | NW deriving (Eq)

  day11a :: String -> Int
  day11a = sumMoves . foldl move (0,0) . map (direction . T.unpack) . T.split (==',') . T.pack

  day11b :: String -> Int
  day11b xs = 0

  direction :: String -> Cardinal
  direction d
    | d == "n" = N
    | d == "ne" = NE
    | d == "e" = E
    | d == "se" = SE
    | d == "s" = S
    | d == "sw" = SW
    | d == "w" = W
    | d == "nw" = NW

  move :: (Int,Int) -> Cardinal -> (Int,Int)
  move (x,y) m
    | m == N = (x, succ y)
    | m == NE = (succ x, succ y)
    | m == E = (succ x, y)
    | m == SE = (succ x, pred y)
    | m == S = (x, pred y)
    | m == SW = (pred x, pred y)
    | m == W = (pred x, y)
    | m == NW = (pred x, succ y)

  sumMoves :: (Int,Int) -> Int
  sumMoves (x,y) = max (abs x) (abs y)
