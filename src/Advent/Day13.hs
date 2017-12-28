module Advent.Day13 (day13a, day13b) where

  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  type Depth = Int
  type Range = Int
  data Layer = Layer Depth Range
  data Tick = Up [Int] | Down [Int] deriving (Show)

  day13a :: String -> Int
  day13a = length . map parseInput . lines

  day13b :: String -> Int
  day13b s = 0

  tick :: Tick -> Tick
  tick xs = case xs of
    (Up xs) -> if atEnd xs then previous xs else next xs
    (Down xs) -> if atStart xs then next xs else previous xs

  atEnd :: [Int] -> Bool
  atEnd xs = case findIndex (==1) xs of
               Nothing -> False
               (Just i) -> let ln = length xs
                               ln' = ln - 1
                           in i == ln'

  atStart :: [Int] -> Bool
  atStart xs = case findIndex (==1) xs of
               Nothing -> False
               (Just i) -> i == 0

  next :: [Int] -> Tick
  next xs = let ln = length xs
            in Up (take ln $ 0:xs)

  previous :: [Int] -> Tick
  previous xs = let ln = length xs
                in Down (take ln $ drop 1 $ xs ++ [0])

  parseInput :: String -> Either ParseError Layer
  parseInput = parse parseLayer ""

  parseLayer :: Parser Layer
  parseLayer = do
    depth <- many1 digit
    _ <- string ":"
    _ <- space
    range <- many1 digit
    return $ Layer (read depth) (read range)
