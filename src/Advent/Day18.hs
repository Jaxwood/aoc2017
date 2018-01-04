module Advent.Day18 (day18a, day18b) where

  import qualified Data.Map.Strict as M
  import Data.List
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.String

  data Instruction =
      Set Char Int
    | SetChar Char Char
    | Add Char Int
    | AddChar Char Char
    | Mul Char Int
    | MulChar Char Char
    | Mod Char Int
    | ModChar Char Char
    | Snd Char
    | Rcv Char
    | JgzChar Char Char
    | Jgz Char Int deriving (Show,Eq)

  day18a :: String -> Int
  day18a s = let is = rights $ map parseInput $ lines s
             in runInstruction M.empty is is

  day18b :: String -> Int
  day18b s = 0

  runInstruction :: M.Map Char Int -> [Instruction] -> [Instruction] -> Int
  runInstruction m s [] = 0
  runInstruction m s (s'@(Set c i):is) = runInstruction (M.insert c i m) s is
  runInstruction m s (s'@(SetChar c c'):is) = runInstruction (M.insert c (lookup' m c') m) s is
  runInstruction m s (s'@(Add c i):is) = runInstruction (update' m (+) c i) s is
  runInstruction m s (s'@(AddChar c c'):is) = runInstruction (update' m (+) c (lookup' m c')) s is
  runInstruction m s (s'@(Mul c i):is) = runInstruction (update' m (*) c i) s is
  runInstruction m s (s'@(MulChar c c'):is) = runInstruction (update' m (*) c (lookup' m c')) s is
  runInstruction m s (s'@(Mod c i):is) = runInstruction (update' m mod c i) s is
  runInstruction m s (s'@(ModChar c c'):is) = runInstruction (update' m mod c (lookup' m c')) s is
  runInstruction m s (s'@(Snd c):is) = runInstruction (M.insert '_' (lookup' m c) m) s is
  runInstruction m s (s'@(Rcv c):is) = if lookup' m c == 0 then runInstruction m s is else lookup' m '_'
  runInstruction m s (s'@(JgzChar c c'):is) = if lookup' m c > 0 then runJgz m s (lookup' m c') s' is else runInstruction m s is
  runInstruction m s (s'@(Jgz c i):is) = if lookup' m c > 0 then runJgz m s i s' is else runInstruction m s is

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

  findIndex' :: Instruction -> [Instruction] -> Int
  findIndex' i is = case findIndex (==i) is of
    Nothing -> error "not found"
    (Just idx) -> idx

  parseInput :: String -> Either ParseError Instruction
  parseInput = parse (choice [try parseSet, try parseSetChar, try parseAdd, try parseAddChar, try parseMul, try parseMulChar, try parseMod, try parseModChar, try parseJgz, try parseJgzChar, try parseSnd, parseRcv]) ""

  parseSet :: Parser Instruction
  parseSet = do
    _ <- string "set"
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', digit]
    return $ Set id (read val)

  parseSetChar :: Parser Instruction
  parseSetChar = do
    _ <- string "set"
    _ <- space
    id <- letter
    _ <- space
    id' <- letter
    return $ SetChar id id'

  parseAdd :: Parser Instruction
  parseAdd = do
    _ <- string "add"
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', digit]
    return $ Add id (read val)

  parseAddChar :: Parser Instruction
  parseAddChar = do
    _ <- string "add"
    _ <- space
    id <- letter
    _ <- space
    id' <- letter
    return $ AddChar id id'

  parseMul :: Parser Instruction
  parseMul = do
    _ <- string "mul"
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', digit]
    return $ Mul id (read val)

  parseMulChar :: Parser Instruction
  parseMulChar = do
    _ <- string "mul"
    _ <- space
    id <- letter
    _ <- space
    id' <- letter
    return $ MulChar id id'

  parseMod :: Parser Instruction
  parseMod = do
    _ <- string "mod"
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', digit]
    return $ Mod id (read val)

  parseModChar :: Parser Instruction
  parseModChar = do
    _ <- string "mod"
    _ <- space
    id <- letter
    _ <- space
    id' <- letter
    return $ ModChar id id'

  parseJgz :: Parser Instruction
  parseJgz = do
    _ <- string "jgz"
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', digit]
    return $ Jgz id (read val)

  parseJgzChar :: Parser Instruction
  parseJgzChar = do
    _ <- string "jgz"
    _ <- space
    id <- letter
    _ <- space
    id' <- letter
    return $ JgzChar id id'

  parseSnd :: Parser Instruction
  parseSnd = do
    _ <- string "snd"
    _ <- space
    id <- letter
    return $ Snd id

  parseRcv :: Parser Instruction
  parseRcv = do
    _ <- string "rcv"
    _ <- space
    id <- letter
    return $ Rcv id
