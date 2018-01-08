module Advent.Day19 (day19a, day19b) where

  data Token = Pipe (Int, Int) | Cross (Int,Int) | Dash (Int,Int) | Letter (Int,Int) Char deriving (Show,Eq)
  data Direction = Down | Up | Left' | Right' deriving (Show,Eq)

  day19a :: String -> String
  day19a s = let tss = map (tokens . indexes) $ zip [0..] $ lines s
                 e = head $ head tss
             in maze tss e Down

  day19b :: String -> Int
  day19b s = let tss = map (tokens . indexes) $ zip [0..] $ lines s
                 e = head $ head tss
             in maze' tss 1 e Down

  maze' :: [[Token]] -> Int -> Token -> Direction -> Int
  maze' tss cnt t d = case move tss (t,d) of
                       (Nothing, d') -> cnt
                       (Just t', d') -> maze' tss (succ cnt) t' d'

  maze :: [[Token]] -> Token -> Direction -> String
  maze tss t d = case move tss (t,d) of
                   (Nothing, d') -> []
                   (Just t'@(Letter _ c), d') -> c:(maze tss t' d')
                   (Just t', d') -> maze tss t' d'

  move :: [[Token]] -> (Token,Direction) -> (Maybe Token, Direction)
  move tss (Pipe (x,y), Down) = (getToken (succ x, y) tss, Down)
  move tss (Letter (x,y) _, Down) = (getToken (succ x, y) tss, Down)
  move tss (Pipe (x,y), Up) = (getToken (pred x, y) tss, Up)
  move tss (Pipe (x,y), Left') = (getToken (x, pred y) tss, Left')
  move tss (Pipe (x,y), Right') = (getToken (x, succ y) tss, Right')
  move tss (Letter (x,y) _, Up) = (getToken (pred x, y) tss, Up)
  move tss (Dash (x,y), Right') = (getToken (x, succ y) tss, Right')
  move tss (Letter (x,y) _, Right') = (getToken (x, succ y) tss, Right')
  move tss (Dash (x,y), Left') = (getToken (x, pred y) tss, Left')
  move tss (Dash (x,y), Up) = (getToken (pred x, y) tss, Up)
  move tss (Dash (x,y), Down) = (getToken (succ x, y) tss, Down)
  move tss (Letter (x,y) _, Left') = (getToken (x, pred y) tss, Left')
  move tss (Cross (x,y), Down) = case getToken (x, succ y) tss of -- left or right?
                                  Nothing -> (getToken (x, pred y) tss, Left')
                                  (Just t) -> (Just t, Right')
  move tss (Cross (x,y), Up) = case getToken (x, succ y) tss of -- left or right?
                                  Nothing -> (getToken (x, pred y) tss, Left')
                                  (Just t) -> (Just t, Right')
  move tss (Cross (x,y), Right') = case getToken (succ x, y) tss of -- up or down?
                                    Nothing -> (getToken (pred x, y) tss, Up)
                                    (Just t) -> (Just t, Down)
  move tss (Cross (x,y), Left') = case getToken (succ x, y) tss of -- up or down?
                                   Nothing -> (getToken (pred x, y) tss, Up)
                                   (Just t) -> (Just t, Down)
  move tss (t,d) = error (show (t,d))

  -- utility

  getToken :: (Int,Int) -> [[Token]] -> Maybe Token
  getToken (x,y) tss = if length tss > x then findToken (x,y) $ tss !! x else Nothing

  findToken :: (Int,Int) -> [Token] -> Maybe Token
  findToken c ts = let t = filter (matchToken c) ts
                   in safeHead t

  safeHead :: [Token] -> Maybe Token
  safeHead ts = case null ts of
    True -> Nothing
    False -> Just (head ts)

  matchToken :: (Int,Int) -> Token -> Bool
  matchToken c (Pipe c') = c == c'
  matchToken c (Cross c') = c == c'
  matchToken c (Dash c') = c == c'
  matchToken c (Letter c' _) = c == c'

  -- parse

  indexes :: (Int, String) -> [((Int, Int), Char)]
  indexes (i,s) = map (\(i',c) -> ((i,i'), c)) $ zip [0..] s

  tokens :: [((Int, Int),Char)] -> [Token]
  tokens [] = []
  tokens ((i,'|'):ts) = (Pipe i):tokens ts
  tokens ((i,'+'):ts) = (Cross i):tokens ts
  tokens ((i,'-'):ts) = (Dash i):tokens ts
  tokens ((i,' '):ts) = tokens ts
  tokens ((i,t):ts) = (Letter i t):tokens ts

