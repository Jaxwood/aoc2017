module Advent.Day11 (day11a, day11b) where

  import qualified Data.Text as T

  data Cardinal = N | NE | SE | S | SW | NW deriving (Eq)
  type X = Int
  type Y = Int
  type Z = Int
  data Cube = Cube X Y Z deriving (Show,Eq)

  day11a :: String -> Int
  day11a = distance . foldl move empty . map (direction . T.unpack) . T.split (==',') . T.pack

  day11b :: String -> Int
  day11b xs = 0

  empty :: Cube
  empty = Cube 0 0 0

  distance :: Cube -> Int
  distance (Cube x y z) = foldl1 max $ map abs [x, y, z]

  direction :: String -> Cardinal
  direction d
    | d == "n" = N
    | d == "ne" = NE
    | d == "se" = SE
    | d == "s" = S
    | d == "sw" = SW
    | d == "nw" = NW

  move :: Cube -> Cardinal -> Cube
  move (Cube x y z) N = (Cube x (succ y) (pred z))
  move (Cube x y z) S = (Cube x (pred y) (succ z))
  move (Cube x y z) NW = (Cube (pred x) (succ y) z)
  move (Cube x y z) NE = (Cube (succ x) y (pred z))
  move (Cube x y z) SW = (Cube (pred x) y (succ z))
  move (Cube x y z) SE = (Cube (succ x) (pred y) z)
