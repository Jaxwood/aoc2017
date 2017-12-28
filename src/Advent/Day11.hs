module Advent.Day11 (day11a, day11b) where

  import qualified Data.Text as T

  data Cardinal = N | NE | SE | S | SW | NW deriving (Eq)
  type X = Int
  type Y = Int
  type Z = Int
  data Cube = Cube X Y Z deriving (Show,Eq)

  day11a :: String -> Int
  day11a = distance . foldl move (empty, empty) . map (direction . T.unpack) . T.split (==',') . T.pack

  day11b :: String -> Int
  day11b = highest . foldl move (empty, empty) . map (direction . T.unpack) . T.split (==',') . T.pack

  empty :: Cube
  empty = Cube 0 0 0

  distance :: (Cube, Cube) -> Int
  distance (Cube x y z, _) = foldl1 max $ map abs [x, y, z]

  highest :: (Cube, Cube) -> Int
  highest (_, Cube x y z) = foldl1 max $ map abs [x, y, z]

  direction :: String -> Cardinal
  direction d
    | d == "n" = N
    | d == "ne" = NE
    | d == "se" = SE
    | d == "s" = S
    | d == "sw" = SW
    | d == "nw" = NW

  maxCube :: (Cube, Cube) -> Cube
  maxCube (c,c') = let m = distance (c,c')
                       m' = distance (c',c)
                   in if m > m' then c else c'

  move :: (Cube, Cube) -> Cardinal -> (Cube, Cube)
  move c@(Cube x y z, _) N = ((Cube x (succ y) (pred z)), maxCube c)
  move c@(Cube x y z, _) S = ((Cube x (pred y) (succ z)), maxCube c)
  move c@(Cube x y z, _) NW = ((Cube (pred x) (succ y) z), maxCube c)
  move c@(Cube x y z, _) NE = ((Cube (succ x) y (pred z)), maxCube c)
  move c@(Cube x y z, _) SW = ((Cube (pred x) y (succ z)), maxCube c)
  move c@(Cube x y z, _) SE = ((Cube (succ x) (pred y) z), maxCube c)
