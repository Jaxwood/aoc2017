module Advent.Day22 (day22a, day22b) where

  import Data.List
  import Data.List.Split as S
  import Data.Function
  import Data.Tuple
  import qualified Data.Map.Strict as M

  type Coord = (Int,Int)
  data Direction = North | West | East | South
  type Position = (Coord, Direction)

  day22a :: String -> Int
  day22a s = direction (((0,0), North), coords s) toggle 0 0

  day22b :: String -> Int
  day22b s = direction' (((0,0), North), coords s) toggle' 0 0

  direction :: (Position, M.Map Coord Char) -> (Char -> Char) -> Int -> Int -> Int
  direction (_,m) fn acc 10000 = acc
  direction (p@(c,d), m) fn acc cnt = case m M.! c of
                                       '#' -> let (c',d') = move p m
                                                  m' = M.adjust fn c m
                                              in direction ((c',d'), update m' c c') fn acc (succ cnt)
                                       '.' -> let (c',d') = move p m
                                                  m' = M.adjust fn c m
                                              in direction ((c',d'), update m' c c') fn (succ acc) (succ cnt)

  direction' :: (Position, M.Map Coord Char) -> (Char -> Char) -> Int -> Int -> Int
  direction' (_,m) fn acc 100 = acc
  direction' (p@(c,d), m) fn acc cnt = case m M.! c of
                                         '#' -> let (c',d') = move p m
                                                    m' = M.adjust fn c m
                                                in direction' ((c',d'), update m' c c') fn acc (succ cnt)
                                         '.' -> let (c',d') = move p m
                                                    m' = M.adjust fn c m
                                                in direction' ((c',d'), update m' c c') fn acc (succ cnt)
                                         'F' -> let (c',d') = move p m
                                                    m' = M.adjust fn c m
                                                in direction' ((c',d'), update m' c c') fn acc (succ cnt)
                                         'W' -> let (c',d') = move p m
                                                    m' = M.adjust fn c m
                                                in direction' ((c',d'), update m' c c') fn (succ acc) (succ cnt)

  update :: M.Map Coord Char -> Coord -> Coord -> M.Map Coord Char
  update m c c' = case M.member c' m of
                    False -> M.insert c' '.' m
                    True -> m

  move :: Position -> M.Map Coord Char -> Position
  move (c,d) m = case m M.! c of
                 '#' -> let d' = right d
                        in (inc c d', d')
                 '.' -> let d' = left d
                        in (inc c d', d')
                 'F' -> let d' = back d
                        in (inc c d', d')
                 'W' -> (inc c d, d)

  inc :: Coord -> Direction -> Coord
  inc (x,y) North = (x,succ y)
  inc (x,y) South = (x, pred y)
  inc (x,y) East = (succ x, y)
  inc (x,y) West = (pred x, y)

  right :: Direction -> Direction
  right North = East
  right East = South
  right South = West
  right West = North

  left :: Direction -> Direction
  left North = West
  left West = South
  left South = East
  left East = North

  back :: Direction -> Direction
  back North = South
  back East = West
  back South = North
  back West = East

  toggle :: Char -> Char
  toggle '#' = '.'
  toggle '.' = '#'

  toggle' :: Char -> Char
  toggle' '#' = 'F'
  toggle' 'F' = '.'
  toggle' '.' = 'W'
  toggle' 'W' = '#'

  -- parse

  coords :: String -> M.Map Coord Char
  coords s = let lns = lines s
                 ln = length lns
                 m = (pred ln) `div` 2
             in M.fromList $ concatMap (uncurry zip . swap) $ zip lns $ reverse $ S.chunksOf ln $ sortBy (compare `on` snd) $ [(x,y) | x <- [-m..m], y <- [-m..m]]

