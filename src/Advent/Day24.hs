module Advent.Day24 (day24a, day24b) where

  import Data.List
  import Data.Monoid
  import Text.Parsec
  import Text.Parsec.String

  data Bridge = Bridge Int Int deriving (Eq,Show)

  instance Monoid Bridge where
    mempty = Bridge 0 0
    mappend (Bridge a b) (Bridge c d) = (Bridge (a+c) (b+d))

  day24a :: String -> [[Bridge]]
  day24a s = let bs = map (right . parseInput) $ lines s
                 zs = filter (match 0) bs
                 zss = map ((\\) bs . pure) zs
             in map (uncurry $ connection 0) $ zip zss zs

  day24b :: String -> Int
  day24b s = 0

  connection :: Int -> [Bridge] -> Bridge -> [Bridge]
  connection i (b:bs) n@(Bridge x y)
    | x == i = n:b:bs
    | y == i = n:b:bs

  match :: Int -> Bridge -> Bool
  match i (Bridge a b)
    | a == i = True
    | b == i = True
    | otherwise = False

  sum' :: Bridge -> Int
  sum' (Bridge a b) = a + b

  sum'' :: [Bridge] -> Int
  sum'' = sum' . mconcat

  -- utility

  right :: Either ParseError Bridge -> Bridge
  right (Left e) = error $ show e
  right (Right r) = r

  -- parse 

  parseInput :: String -> Either ParseError Bridge
  parseInput = parse parseBridge ""

  parseBridge :: Parser Bridge
  parseBridge = do
    str <- many1 digit
    _ <- char '/'
    str' <- many1 digit
    return $ Bridge (read str) (read str')
