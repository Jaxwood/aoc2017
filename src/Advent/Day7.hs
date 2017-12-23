module Advent.Day7 (day7a, day7b) where

  import Data.List
  import Data.Either
  import Data.Tree
  import Text.Parsec
  import Text.Parsec.String

  data Tower = Tower String Int [String] deriving (Eq, Show)

  day7a :: String -> String
  day7a = (\(Tower a _ _) -> a) . root . rights . map parseInput . lines

  day7b :: String -> Int
  day7b t = let (Node _ s) = (buildTree . rights . map parseInput . lines) t
                tt = map (\(Node a _) -> snd a) s 
                ts = map sumChildren s
                gs = (maximum ts) - (minimum ts)
                o = head $ concatMap id $ filter (\x -> length x == 1) $ group $ sort ts
                (a,b) = head $ filter (\(a,b) -> b == o) $ zip tt ts
            in a - gs

  sumChildren :: Tree (String, Int) -> Int
  sumChildren (Node (_,i) is) = i + sum (map sumChildren is)

  buildTree :: [Tower] -> Tree (String, Int)
  buildTree ts = let (Tower a b c) = root ts
                 in Node (a,b) $ getChildren c ts

  getChildren :: [String] -> [Tower] -> [Tree (String, Int)]
  getChildren ss tt = let tt' = map (\s -> (!!!) s tt) ss
                      in map (getChild tt) tt'

  getChild :: [Tower] -> Maybe Tower -> Tree (String, Int)
  getChild tt t = case t of
      Nothing -> Node ("",0) []
      Just (Tower a b c) -> Node (a, b) $ getChildren c tt

  root :: [Tower] -> Tower
  root xs = head $ filter (not . elem' xs) xs

  elem' :: [Tower] -> Tower -> Bool
  elem' ts (Tower t _ _) = any (\(Tower _ _ ts') -> t `elem` ts') ts

  (!!!) :: String -> [Tower] -> Maybe Tower
  (!!!) a ts = find (\(Tower a' _ _) -> a == a') ts

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
