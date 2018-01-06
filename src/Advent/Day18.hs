module Advent.Day18 (day18a, day18b) where

  import qualified Data.Map.Strict as M
  import Data.Char
  import Data.List as L
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  type Queue = [Int]
  type Times = Int
  data Program a b = Program [a] b (Maybe Queue) Times
  data Value = Number Int | Register Char deriving (Show,Eq) 
  data Instruction = Set Char Value | Add Char Value | Mul Char Value | Mod Char Value | Snd Char | Rcv Char | Jgz Char Value deriving (Show,Eq)

  instance Functor (Program a) where
    fmap fn (Program (x:is) m q t) = Program is (fn m) q t

  day18a :: String -> Int
  day18a s = let is = rights $ map parseInput $ lines s
                 (Program _ m _ _) = runInstruction (Program is M.empty Nothing 0) is
             in lookup' m '_'

  day18b :: String -> Int
  day18b s = let is = rights $ map parseInput $ lines s
                 p = Program is (M.singleton 'p' 0) Nothing 0
                 p' = Program is (M.singleton 'p' 1) Nothing 0
             in runProgram p p' is

  runProgram :: Program Instruction (M.Map Char Int) -> Program Instruction (M.Map Char Int) -> [Instruction] -> Int
  runProgram (Program _ _ (Just []) _) (Program _ _ (Just []) t) _ = 1
  runProgram p p' is = 0

  runInstruction :: Program Instruction (M.Map Char Int) -> [Instruction] -> Program Instruction (M.Map Char Int)
  runInstruction p@(Program [] _ _ _) _ = p
  runInstruction p@(Program ((Set c i):is) _ _ _) is' = runInstruction (fmap (\m -> M.insert c (lookup'' m i) m) p) is'
  runInstruction p@(Program ((Add c i):is) _ _ _) is'  = runInstruction (fmap (\m -> update' m (+) c (lookup'' m i)) p) is'
  runInstruction p@(Program ((Mul c i):is) _ _ _) is' = runInstruction (fmap (\m -> update' m (*) c (lookup'' m i)) p) is'
  runInstruction p@(Program ((Mod c i):is) _ _ _) is' = runInstruction (fmap (\m -> update' m mod c (lookup'' m i)) p) is'
  runInstruction p@(Program ((Snd c):is) _ _ _) is' = runInstruction (fmap (\m -> M.insert '_' (lookup' m c) m) p) is'
  runInstruction p@(Program ((Rcv c):is) m _ _) is' = if lookup' m c == 0 then runInstruction (fmap id p) is' else p
  runInstruction p@(Program (s@(Jgz c i):is) m q t) is' = if lookup' m c > 0 then runInstruction (Program (runJgz is' (lookup'' m i) s) m q t) is' else runInstruction (fmap id p) is'

  runJgz :: [Instruction] -> Int -> Instruction -> [Instruction]
  runJgz is v i = let idx = findIndex' i is
                      s' = drop (idx + v) is
                  in s'

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

  enqueue :: Maybe Queue -> Int -> Maybe Queue
  enqueue Nothing i = Just [i]
  enqueue (Just is) i = Just (i:is)

  dequeue :: Maybe Queue -> Maybe (Int, Maybe Queue)
  dequeue Nothing = Nothing
  dequeue (Just is) = case null is of
    True -> Nothing
    False -> Just (last is, Just $ init is)

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
