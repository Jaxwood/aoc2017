module Advent.Day13 (day13a, day13b) where

  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Depth = Int
  type Range = Int
  data Tick = Up [Int] | Down [Int] deriving (Show, Eq)
  data Layer = Layer Depth Range Tick deriving (Show, Eq)

  day13a :: String -> Int
  day13a = firewall 0 6 0 . rights . map parseInput . lines

  day13b :: String -> Int
  day13b s = 0

  firewall :: Int -> Int -> Int -> [Layer] -> Int
  firewall c m acc l 
    | c == m = acc + collision c l
    | otherwise = let acc' = acc + collision c l
                  in firewall (succ c) m acc' (fmap tick l)

  collision :: Int -> [Layer] -> Int
  collision c ls = case find (\(Layer a _ _) -> a == c) ls of
                     Nothing -> 0
                     Just (Layer a b (Up c)) -> if c !! 0 == 1 then a * b else 0
                     Just (Layer a b (Down c)) -> if c !! 0 == 1 then a * b else 0

  tick :: Layer -> Layer
  tick (Layer a b xs) = case xs of
    (Up xs) -> if atEnd xs then (Layer a b $ previous xs) else (Layer a b $ next xs)
    (Down xs) -> if atStart xs then (Layer a b $ next xs) else (Layer a b $previous xs)

  atEnd :: [Int] -> Bool
  atEnd = (==1) . last

  atStart :: [Int] -> Bool
  atStart = (==1) . head

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
    return $ Layer (read depth) (read range) (Up $ init $ 1:(replicate (read range) 0))
