module Advent.Day7 (day7a, day7b, Tower(Tower)) where

  import Data.List
  import Data.Either
  import Text.Parsec
  import Text.Parsec.String

  data Tower = Tower String Int [String] deriving (Eq, Show)

  day7a :: String -> String
  day7a = withId . head . parent . rights . map parseInput . lines

  day7b :: String -> String
  day7b = id

  withId :: Tower -> String
  withId (Tower s _ _) = s

  parent :: [Tower] -> [Tower]
  parent xs = filter (\x -> x `elem'` xs == False) xs

  elem' :: Tower -> [Tower] -> Bool
  elem' (Tower t _ _) ts = any (\(Tower _ _ ts') -> t `elem` ts') ts

  parseInput :: String -> Either ParseError Tower
  parseInput a =  parse (choice [try parseNode, parseLeaf]) "" a

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
