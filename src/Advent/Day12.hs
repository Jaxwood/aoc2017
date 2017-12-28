module Advent.Day12 (day12a, day12b) where

  import Data.Array
  import Data.Graph (Graph, reachable)
  import Text.Parsec
  import Text.Parsec.String
  import Text.ParserCombinators.Parsec.Error

  data Node = Node Int [Int]

  day12a :: Int -> String -> Int
  day12a i s = hasPath $ graph i $ map (neighbors . parseInput) $ lines s

  day12b :: String -> Int
  day12b s = 0

  hasPath :: Graph -> Int
  hasPath g = length $ reachable g 0

  graph :: Int -> [[Int]] -> Graph
  graph i is = listArray (0,i) is

  neighbors :: Either ParseError Node -> [Int]
  neighbors s = case s of
    Right (Node i is) -> is
    Left msg -> error (foldl (\acc msg -> acc ++ messageString msg) "" $ errorMessages msg)

  parseInput :: String -> Either ParseError Node
  parseInput =  parse parseNode ""

  parseNode :: Parser Node
  parseNode = do
    node <- many1 digit
    _ <- space
    _ <- string "<->"
    _ <- space
    edges <- sepBy (many1 digit) (string ", ")
    return $ Node (read node) $ map read edges
