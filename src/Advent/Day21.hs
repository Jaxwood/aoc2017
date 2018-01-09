module Advent.Day21 (day21a, day21b) where

  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  data Rule = Rule [[Char]] [[Char]] deriving (Show,Eq)

  day21a :: String -> [Rule]
  day21a = map (right . parseInput) . lines

  day21b :: String -> Int
  day21b s = 0

  pattern :: [[Char]] -> [[[Char]]]
  pattern iss = (flip' iss):(flip'' iss):(rotate iss)

  rotate :: [[Char]] -> [[[Char]]]
  rotate iss = take 4 $ iterate (map reverse . transpose) iss

  flip' :: [[Char]] -> [[Char]]
  flip' = reverse

  flip'' :: [[Char]] -> [[Char]]
  flip'' = map reverse

  size :: [[Char]] -> Int
  size = length

  on :: [[Char]] -> Int
  on = length . filter (=='#') . concat

  right :: Either ParseError Rule -> Rule
  right e = case e of
    (Right r) -> r
    (Left err) -> error $ show err

  -- parse

  parseInput :: String -> Either ParseError Rule 
  parseInput = parse (choice [parseRule]) ""

  parseRule :: Parser Rule
  parseRule = do
    from <- sepBy (many1 $ choice [try $ char '.', char '#']) $ char '/'
    _ <- space 
    _ <- string "=>"
    _ <- space
    to <- sepBy (many1 $ choice [try $ char '.', char '#']) $ char '/'
    return $ Rule from to
