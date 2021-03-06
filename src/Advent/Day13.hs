module Advent.Day13 (day13a, day13b) where

  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Depth = Int
  type Range = Int
  data Tick = Up Int | Down Int deriving (Show, Eq)
  data Layer = Layer Depth Range Tick deriving (Show, Eq)

  day13a :: String -> Int
  day13a = firewall (*) 0 6 0 . rights . map parseInput . lines

  day13b :: String -> Int
  day13b = firewall' 0 0 6 0 . rights . map parseInput . lines

  firewall :: (Int -> Int -> Int) -> Int -> Int -> Int -> [Layer] -> Int
  firewall fn c m acc l 
    | c == m = acc + collision fn c l
    | otherwise = let acc' = acc + collision fn c l
                  in firewall fn (succ c) m acc' (fmap tick l)

  firewall' :: Int -> Int -> Int -> Int -> [Layer] -> Int
  firewall' d c m acc l = let tick' = fmap tick l
                              res = firewall (\x y -> 1) c m acc l
                          in if res == 0 then d else firewall' (succ d) c m acc tick'

  collision :: (Int -> Int -> Int) -> Int -> [Layer] -> Int
  collision fn c ls = case find (\(Layer a _ _) -> a == c) ls of
                     Nothing -> 0
                     Just (Layer a b (Up c)) -> if start c then fn a b else 0
                     Just (Layer a b (Down c)) -> if start c then fn a b else 0

  tick :: Layer -> Layer
  tick (Layer a b xs) = case xs of
    (Up xs) -> if end b xs then (Layer a b $ previous xs) else (Layer a b $ next xs)
    (Down xs) -> if start xs then (Layer a b $ next xs) else (Layer a b $previous xs)

  end :: Int -> Int -> Bool
  end a b =  a == b

  start :: Int -> Bool
  start = (==1)

  next :: Int -> Tick
  next xs = Up (succ xs)

  previous :: Int -> Tick
  previous xs = Down (pred xs)

  parseInput :: String -> Either ParseError Layer
  parseInput = parse parseLayer ""

  parseLayer :: Parser Layer
  parseLayer = do
    depth <- many1 digit
    _ <- string ":"
    _ <- space
    range <- many1 digit
    return $ Layer (read depth) (read range) (Up 1)
