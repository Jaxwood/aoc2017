module Advent.Day23 (day23a, day23b) where

  import Data.List
  import qualified Data.Map.Strict as M
  import Data.Char
  import Text.Parsec
  import Text.Parsec.String

  data Value = Number Int | Register Char deriving (Show,Eq) 
  data Instruction = Set Char Value | Add Char Value | Mul Char Value | Sub Char Value | Jnz Value Value deriving (Eq)

  day23a :: String -> Int
  day23a s = let is = map (right . parseInput) $ lines s
             in run (M.fromList [(x,0)|x<-['a'..'h']]) is is

  day23b :: String -> Int
  day23b s = let is = map (right . parseInput) $ lines s
                 m = M.fromList [(x,0)|x<-['a'..'h']]
                 m' = run (M.adjust (const 1) 'a' m) is is
             in m M.! 'h'

  run :: M.Map Char Int -> [Instruction] -> [Instruction] -> Int
  run _ [] _ = 0
  run m ((Set x v):is) is' = run (update x v m const) is is'
  run m ((Sub x v):is) is' = run (update x v m (flip (-))) is is'
  run m ((Add x v):is) is' = run (update x v m (flip (+))) is is'
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
    instruction <- choice [try $ string "mul", try $ string "add", try $ string "set", string "sub"]
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', try digit, letter]
    return $ case instruction of
               "mul" -> Mul id (if any isDigit val then Number $ read val else Register $ head val)
               "add" -> Add id (if any isDigit val then Number $ read val else Register $ head val)
               "set" -> Set id (if any isDigit val then Number $ read val else Register $ head val)
               "sub" -> Sub id (if any isDigit val then Number $ read val else Register $ head val)

  parseJnz :: Parser Instruction
  parseJnz = do
    _ <- string "jnz"
    _ <- space
    id <- many1 $ choice [try $ char '-', try digit, letter]
    _ <- space
    val <- many1 $ choice [try $ char '-', try digit, letter]
    return $ Jnz (if any isDigit id then Number $ read id else Register $ head id) (if any isDigit val then Number $ read val else Register $ head val)
