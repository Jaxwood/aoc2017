module Advent.Day18 (day18a, day18b) where

  import qualified Data.Map.Strict as M
  import Data.Char
  import Data.List as L
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Queue = [Int]
  data Program = Program [Instruction] (M.Map Char Int) Queue
  data Value = Number Int | Register Char deriving (Show,Eq) 
  data Instruction = Set Char Value | Add Char Value | Mul Char Value | Mod Char Value | Snd Char | Rcv Char | Jgz Char Value deriving (Show,Eq)

  day18a :: String -> Int
  day18a s = let is = rights $ map parseInput $ lines s
             in runInstruction (Program is M.empty []) is

  day18b :: String -> Int
  day18b s = let p = Program [] (M.singleton 'p' 0) []
                 p' = Program [] (M.singleton 'p' 1) []
             in 0


  runInstruction :: Program -> [Instruction] -> Int
  runInstruction p [] = 0
  runInstruction p@(Program s m q) ((Set c i):is) = runInstruction (Program s (M.insert c (lookup'' m i) m) q) is
  runInstruction p@(Program s m q) ((Add c i):is) = runInstruction (Program s (update' m (+) c (lookup'' m i)) q) is
  runInstruction p@(Program s m q) ((Mul c i):is) = runInstruction (Program s (update' m (*) c (lookup'' m i)) q) is
  runInstruction p@(Program s m q) ((Mod c i):is) = runInstruction (Program s (update' m mod c (lookup'' m i)) q) is
  runInstruction p@(Program s m q) ((Snd c):is) = runInstruction (Program s (M.insert '_' (lookup' m c) m) q) is
  runInstruction p@(Program s m q) ((Rcv c):is) = if lookup' m c == 0 then runInstruction p is else lookup' m '_'
  runInstruction p@(Program s m q) (s'@(Jgz c i):is) = if lookup' m c > 0 then runJgz p (lookup'' m i) s' is else runInstruction p is

  runJgz :: Program -> Int -> Instruction -> [Instruction] -> Int
  runJgz p@(Program s m _) v i is = let idx = findIndex' i s
                                        s' = drop (idx + v) s
                                    in runInstruction p s'

  update' :: M.Map Char Int -> (Int -> Int -> Int) -> Char -> Int -> M.Map Char Int
  update' m fn c i = M.insert c (fn (lookup' m c) i) m

  lookup' :: M.Map Char Int -> Char -> Int
  lookup' m c = case M.lookup c m of
    (Just i) -> i
    Nothing -> 0

  lookup'' :: M.Map Char Int -> Value -> Int
  lookup'' m (Number i) = i
  lookup'' m (Register c) = lookup' m c

  findIndex' :: Instruction -> [Instruction] -> Int
  findIndex' i is = case findIndex (==i) is of
    (Just idx) -> idx
    Nothing -> error "not found"

  enqueue :: Queue -> Int -> Queue
  enqueue is i = (i:is)

  dequeue :: Queue -> Maybe (Int, Queue)
  dequeue is = case null is of
    True -> Nothing
    False -> Just (last is, init is)

  parseInput :: String -> Either ParseError Instruction
  parseInput = parse (choice [try parseInstruction, parseInstruction']) ""

  parseInstruction :: Parser Instruction
  parseInstruction = do
    instruction <- choice [try $ string "mul", try $ string "mod", try $ string "jgz", try $ string "add", string "set"]
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', try digit, letter]
    return $ case instruction of
               "mul" -> Mul id (if any isDigit val then Number $ read val else Register $ head val)
               "add" -> Add id (if any isDigit val then Number $ read val else Register $ head val)
               "set" -> Set id (if any isDigit val then Number $ read val else Register $ head val)
               "mod" -> Mod id (if any isDigit val then Number $ read val else Register $ head val)
               "jgz" -> Jgz id (if any isDigit val then Number $ read val else Register $ head val)

  parseInstruction' :: Parser Instruction
  parseInstruction' = do
    instruction <- choice [try $ string "snd", string "rcv"]
    _ <- space
    id <- letter
    return $ case instruction of
      "snd" -> Snd id
      "rcv" -> Rcv id
