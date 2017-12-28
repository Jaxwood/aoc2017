module Advent.Day13 (day13a, day13b, Layer(Layer)) where

  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Depth = Int
  type Range = [Int]
  data Layer = Layer Depth Range deriving (Show, Eq)
  data Tick = Up [Int] | Down [Int] deriving (Show)

  day13a :: String -> [Layer]
  day13a = rights . map parseInput . lines

  day13b :: String -> Int
  day13b s = 0

  firewall :: Layer -> [Int]
  firewall l = []

  tick :: Tick -> Tick
  tick xs = case xs of
    (Up xs) -> if atEnd xs then previous xs else next xs
    (Down xs) -> if atStart xs then next xs else previous xs

  atEnd :: [Int] -> Bool
  atEnd xs = case findIndex (==1) xs of
               Nothing -> False
               (Just i) -> i == (pred $ length xs)

  atStart :: [Int] -> Bool
  atStart xs = case findIndex (==1) xs of
               Nothing -> False
               (Just i) -> i == 0

  next :: [Int] -> Tick
  next xs = Up (init $ 0:xs)

  previous :: [Int] -> Tick
  previous xs = Down (tail $ xs ++ [0])

  parseInput :: String -> Either ParseError Layer
  parseInput = parse parseLayer ""

  parseLayer :: Parser Layer
  parseLayer = do
    depth <- many1 digit
    _ <- string ":"
    _ <- space
    range <- many1 digit
    return $ Layer (read depth) (init $ 1:(replicate (read range) 0))
