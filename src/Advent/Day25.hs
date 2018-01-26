module Advent.Day25 (day25a, day25b) where

  import Data.List.Split
  import qualified Data.Vector as V
  import Text.Parsec
  import Text.Parsec.String

  data TuringState = TuringState Char (Int,Direction,Char) (Int,Direction,Char) deriving (Eq,Show)
  data Direction = L | R deriving (Show,Eq)

  day25a :: String -> Int
  day25a s = let a = head $ map (right . parseStart) $ lines s
                 b = head $ map (right . parseTimes) $ drop 1 $ lines s
                 states = map parseStates $ chunksOf 10 $ drop 2 $ lines s
                 start = next states a
                 (v, _, _) = last $ take b  $ iterate (transition states) ((V.singleton 0), 0, start)
             in V.sum v

  day25b :: String -> Int
  day25b s = 0

  next :: [TuringState] -> Char -> TuringState
  next ts c = head $ filter (\(TuringState a _ _) -> a == c) ts

  transition :: [TuringState] -> (V.Vector Int, Int, TuringState) -> (V.Vector Int, Int, TuringState)
  transition ts (is, idx, (TuringState _ (v,d,n) (v',d',n'))) =
    let x = is V.!? idx
    in case x of
      Just 0 -> (updateVector is idx v d, nextIdx d idx, next ts n)
      Just 1 -> (updateVector is idx v' d', nextIdx d' idx, next ts n')
      Nothing ->
        if idx > 0
        then
          (updateVector (V.snoc is 0) idx v d, nextIdx d idx, next ts n)
        else
          (updateVector (V.cons 0 is) 0 v d, nextIdx d 0, next ts n)

  nextIdx :: Direction -> Int -> Int
  nextIdx R idx = succ idx
  nextIdx L idx = pred idx

  updateVector :: V.Vector Int -> Int -> Int -> Direction -> V.Vector Int
  updateVector is idx v d = case d of 
                              R -> (V.//) is [(idx,v)]
                              L -> (V.//) is [(idx,v)]

  -- utility

  right :: Either ParseError a -> a
  right (Left e) = error $ show e
  right (Right r) = r

  -- parse 
  parseStates :: [String] -> TuringState
  parseStates (_:n:_:w:m:s:_:w':m':s':[]) = let n' = right $ parse parseStateName "" n
                                                w'' = right $ parse parseStateWrite "" w
                                                w''' = right $ parse parseStateWrite "" w'
                                                m'' = right $ parse parseStateMove "" m
                                                m''' = right $ parse parseStateMove "" m'
                                                s'' = right $ parse parseStateNext "" s
                                                s''' = right $ parse parseStateNext "" s'
                                            in TuringState n' (w'',m'',s'') (w''',m''',s''')


  parseStart :: String -> Either ParseError Char
  parseStart = parse parseStartState ""

  parseTimes :: String -> Either ParseError Int
  parseTimes = parse parseTimesState ""

  parseStateName :: Parser Char
  parseStateName = do
    _ <- string "In state "
    a <- many1 letter
    _ <- char ':'
    return $ head a

  parseStateWrite :: Parser Int
  parseStateWrite = do
    _ <- string "    - Write the value "
    a <- many1 digit
    _ <- char '.'
    return $ read a

  parseStateMove :: Parser Direction
  parseStateMove = do
    _ <- string "    - Move one slot to the "
    a <- choice [try $ string "right", string "left"]
    _ <- char '.'
    return $ if a == "left" then L else R

  parseStateNext :: Parser Char
  parseStateNext = do
    _ <- string "    - Continue with state "
    a <- many1 letter
    _ <- char '.'
    return $ head a

  parseStartState :: Parser Char
  parseStartState = do
    _ <- string "Begin in state "
    a <- many1 letter
    _ <- char '.'
    return $ head a

  parseTimesState :: Parser Int
  parseTimesState = do
    _ <- string "Perform a diagnostic checksum after "
    a <- many1 digit
    _ <- string " steps."
    return $ (read a)

