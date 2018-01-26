module Advent.Day24 (day24a, day24b) where

  import Data.List
  import Data.Tree
  import Text.Parsec
  import Text.Parsec.String

  day24a :: String -> Int
  day24a s = let bs = map (right . parseInput) $ lines s
                 zs = filter (match 0) bs
                 forest = map (\z -> Node z $ getChildren (bs \\ pure z) z 0) zs
             in maximum $ concatMap (sum' 0) forest

  day24b :: String -> Int
  day24b s = 0

  getChildren :: [(Int,Int)] -> (Int,Int) -> Int -> Forest (Int,Int)
  getChildren bs b i = map (\x -> Node x $ getChildren (bs \\ pure x) x (next i b)) $ filter (match (next i b)) bs

  sum' :: Int -> Tree (Int,Int) -> [Int]
  sum' s (Node (a,b) as) = let s' = a+b+s
                           in s':(concatMap (sum' s') as)

  match :: Int -> (Int,Int) -> Bool
  match i (a,b)
    | a == i = True
    | b == i = True
    | otherwise = False

  next :: Int -> (Int,Int) -> Int
  next i (a,b)
    | a == i = b
    | b == i = a
    | otherwise = error "not found"

  -- utility

  right :: Either ParseError (Int,Int) -> (Int,Int)
  right (Left e) = error $ show e
  right (Right r) = r

  -- parse 

  parseInput :: String -> Either ParseError (Int,Int)
  parseInput = parse parseBridge ""

  parseBridge :: Parser (Int,Int)
  parseBridge = do
    str <- many1 digit
    _ <- char '/'
    str' <- many1 digit
    return $ (read str, read str')
