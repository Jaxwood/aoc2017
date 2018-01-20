module Advent.Day23 (day23a, day23b) where

  import Data.List
  import qualified Data.Map.Strict as M
  import Data.Char
  import Text.Parsec
  import Text.Parsec.String

  data Value = Number Int | Register Char deriving (Show,Eq) 
  data Instruction = Set Char Value | Mul Char Value | Sub Char Value | Jnz Value Value deriving (Eq)

  day23a :: String -> Int
  day23a s = let is = map (right . parseInput) $ lines s
             in run (M.fromList [(x,0)|x<-['a'..'h']]) is is

  -- Since the algorithm is super slow a better way is to analyze what it is actually doing.
  -- There is 3 loops inside the algorithm. However the essential part is the sub -17 at the end
  -- this basically means that the outer loops runs approx 1000 times (since c is 17000 bigger than b). 
  -- Inside the inner loop the interesting part is the `set f 0` as that will lead to increment of the h registry.
  -- The condition for setting `set f 0` is to find a number can be divided by `b`. The only numbers that doesn't
  -- fulfill this is prime numbers - if we can find all non prime numbers it should lead to the correct answer.

  day23b :: Int
  day23b = length $ filter (not . isPrime) $ take 1001 $ [x|x<-[109900,109917..]]

  isPrime :: Int -> Bool
  isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]

  run :: M.Map Char Int -> [Instruction] -> [Instruction] -> Int
  run _ [] _ = 0
  run m ((Set x v):is) is' = run (update x v m const) is is'
  run m ((Sub x v):is) is' = run (update x v m (flip (-))) is is'
  run m ((Mul x v):is) is' = succ $ run (update x v m (flip (*))) is is'
  run m (i@(Jnz (Number x) v):is) is' = if x == 0 then run m is is' else run m (jump m v i is') is'
  run m (i@(Jnz (Register x) v):is) is' = if m M.! x == 0 then run m is is' else run m (jump m v i is') is'

  update :: Char -> Value -> M.Map Char Int -> (Int -> Int -> Int) ->  M.Map Char Int
  update c (Number n) m fn = M.adjust (fn n) c m
  update c (Register n) m fn = let n' = m M.! n
                               in M.adjust (fn n') c m

  jump :: M.Map Char Int -> Value -> Instruction -> [Instruction] -> [Instruction]
  jump m (Number n) i is = case findIndex (==i) is of
                             Nothing -> error "not found"
                             (Just a) -> drop (a + n) is
  jump m (Register n) i is = let n' = m M.! n
                             in case findIndex (==i) is of
                               Nothing -> error "not found"
                               (Just a) -> drop (a + n') is

  -- utility

  right :: Either ParseError Instruction -> Instruction
  right (Left e) = error $ show e
  right (Right r) = r

  -- parse 

  parseInput :: String -> Either ParseError Instruction
  parseInput = parse (choice [try parseInstruction, parseJnz]) ""

  parseInstruction :: Parser Instruction
  parseInstruction = do
    instruction <- choice [try $ string "mul", try $ string "set", string "sub"]
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', try digit, letter]
    return $ case instruction of
               "mul" -> Mul id $ value val
               "set" -> Set id $ value val
               "sub" -> Sub id $ value val

  value :: String -> Value
  value val = if any isDigit val then Number $ read val else Register $ head val

  parseJnz :: Parser Instruction
  parseJnz = do
    _ <- string "jnz"
    _ <- space
    id <- many1 $ choice [try $ char '-', try digit, letter]
    _ <- space
    val <- many1 $ choice [try $ char '-', try digit, letter]
    return $ Jnz (if any isDigit id then Number $ read id else Register $ head id) (if any isDigit val then Number $ read val else Register $ head val)
