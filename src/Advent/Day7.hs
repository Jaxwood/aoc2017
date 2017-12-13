module Advent.Day7 (day7a, day7b) where

  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  data Tower = Tower String Int [String] deriving (Eq, Show)

  day7a :: String -> [Either ParseError Tower]
  day7a = map parseInput . lines

  day7b :: String -> String
  day7b = id
    
  parseInput :: String -> Either ParseError Tower
  parseInput =  parse (choice [try parseNode, parseLeaf]) ""

  parseNode :: Parser Tower
  parseNode = do
    l <- many1 letter
    _ <- space
    _ <- char '('
    d <- many1 digit
    _ <- char ')'
    _ <- space
    _ <- string "->"
    _ <- space
    leafs <- sepBy (many1 letter) (string ", ")
    return $ Tower l (read d) leafs

  parseLeaf :: Parser Tower
  parseLeaf = do
    l <- many1 letter
    _ <- space
    _ <- char '('
    d <- many1 digit
    _ <- char ')'
    return $ Tower l (read d) []
