module Advent.Day18 (day18a, day18b) where

  import qualified Data.Map.Strict as M
  import Data.Char
  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  data Value = Number Int | Register Char deriving (Show,Eq)
  data Instruction = Set Char Value | Add Char Value | Mul Char Value | Mod Char Value | Snd Char | Rcv Char | Jgz Char Value deriving (Show,Eq)

  day18a :: String -> Int
  day18a s = let is = rights $ map parseInput $ lines s
             in runInstruction M.empty is is

  day18b :: String -> Int
  day18b s = 0

  runInstruction :: M.Map Char Int -> [Instruction] -> [Instruction] -> Int
  runInstruction m s [] = 0
  runInstruction m s ((Set c i):is) = runInstruction (M.insert c (lookup'' m i) m) s is
  runInstruction m s ((Add c i):is) = runInstruction (update' m (+) c (lookup'' m i)) s is
  runInstruction m s ((Mul c i):is) = runInstruction (update' m (*) c (lookup'' m i)) s is
  runInstruction m s ((Mod c i):is) = runInstruction (update' m mod c (lookup'' m i)) s is
  runInstruction m s ((Snd c):is) = runInstruction (M.insert '_' (lookup' m c) m) s is
  runInstruction m s ((Rcv c):is) = if lookup' m c == 0 then runInstruction m s is else lookup' m '_'
  runInstruction m s (s'@(Jgz c i):is) = if lookup' m c > 0 then runJgz m s (lookup'' m i) s' is else runInstruction m s is

  runJgz :: M.Map Char Int -> [Instruction] -> Int -> Instruction -> [Instruction] -> Int
  runJgz m s v i is = let idx = findIndex' i s
                          s' = drop (idx + v) s
                      in runInstruction m s s'

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
