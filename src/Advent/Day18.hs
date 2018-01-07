module Advent.Day18 (day18a, day18b) where

  import qualified Data.Map.Strict as M
  import Data.Char
  import Data.Tuple (swap)
  import Data.List as L
  import Data.Either (rights)
  import Text.Parsec
  import Text.Parsec.Error (messageString, errorMessages)
  import Text.Parsec.String

  type Queue = [Int]
  type Times = Int
  data Program a b = Program [a] b Times deriving (Eq,Show)
  data Value = Number Int | Register Char deriving (Show,Eq) 
  data Instruction = Set Char Value | Add Char Value | Mul Char Value | Mod Char Value | Snd Char | Rcv Char | Jgz Value Value deriving (Show,Eq)

  instance Functor (Program a) where
    fmap fn (Program (x:is) m t) = Program is (fn m) t

  day18a :: String -> Int
  day18a s = let is = rights $ map parseInput $ lines s
                 (Program _ m _) = runInstruction (Program is M.empty 0) is
             in lookup' m '_'

  day18b :: String -> Int
  day18b s = let is = map (right . parseInput) $ lines s
                 p = Program is (M.singleton 'p' 0) 0
                 p' = Program is (M.singleton 'p' 1) 0
             in runProgram p p' (Nothing,Nothing) is

  right :: Either ParseError Instruction -> Instruction
  right (Right i) = i
  right (Left e) = error (foldl1 (++) $ map messageString $ errorMessages e)

  runProgram :: Program Instruction (M.Map Char Int) -> Program Instruction (M.Map Char Int) -> (Maybe Queue, Maybe Queue) -> [Instruction] -> Int
  runProgram p (Program _ _ t) q@(Just [], Just []) _ = t
  runProgram (Program [] _ _) (Program [] _ t) _ _ = t
  runProgram p p' q is = let (a,q') = executeProgram p q is
                             (b,q'') = executeProgram p' (swap q') is
                         in runProgram a b (swap q'') is

  executeProgram :: Program Instruction (M.Map Char Int) -> (Maybe Queue, Maybe Queue) -> [Instruction] -> (Program Instruction (M.Map Char Int), (Maybe Queue, Maybe Queue))
  executeProgram p@(Program [] _ _) q  _ = (p, q)
  executeProgram p@(Program ((Set c i):is) _ _ ) q is' = executeProgram (fmap (\m -> M.insert c (lookup'' m i) m) p) q is'
  executeProgram p@(Program ((Add c i):is) _ _ ) q is'  = executeProgram (fmap (\m -> update' m (+) c (lookup'' m i)) p) q is'
  executeProgram p@(Program ((Mul c i):is) _ _ ) q is' = executeProgram (fmap (\m -> update' m (*) c (lookup'' m i)) p) q is'
  executeProgram p@(Program ((Mod c i):is) _ _ ) q is' = executeProgram (fmap (\m -> update' m mod c (lookup'' m i)) p) q is'
  executeProgram p@(Program (s@(Jgz (Register c) i):is) m t) q is' = if lookup' m c > 0 then executeProgram (Program (runJgz is' (lookup'' m i) s) m t) q is' else executeProgram (fmap id p) q is'
  executeProgram p@(Program (s@(Jgz (Number n) i):is) m t) q is' = if n > 0 then executeProgram (Program (runJgz is' (lookup'' m i) s) m t) q is' else executeProgram (fmap id p) q is'
  executeProgram p@(Program ((Snd c):is) m t) (q,q') is' = executeProgram (Program is m (succ t)) (q,(enqueue q' (lookup' m c))) is'
  executeProgram p@(Program ((Rcv c):is) m t) (q,q') is' = case dequeue q of
      Nothing -> (p,(q,q'))
      (Just (i, q'')) -> executeProgram (Program is (M.insert c i m) t) (Just q'',q') is'

  runInstruction :: Program Instruction (M.Map Char Int) -> [Instruction] -> Program Instruction (M.Map Char Int)
  runInstruction p@(Program [] _ _ ) _ = p
  runInstruction p@(Program ((Set c i):is) _ _ ) is' = runInstruction (fmap (\m -> M.insert c (lookup'' m i) m) p) is'
  runInstruction p@(Program ((Add c i):is) _ _ ) is'  = runInstruction (fmap (\m -> update' m (+) c (lookup'' m i)) p) is'
  runInstruction p@(Program ((Mul c i):is) _ _ ) is' = runInstruction (fmap (\m -> update' m (*) c (lookup'' m i)) p) is'
  runInstruction p@(Program ((Mod c i):is) _ _ ) is' = runInstruction (fmap (\m -> update' m mod c (lookup'' m i)) p) is'
  runInstruction p@(Program ((Snd c):is) _ _ ) is' = runInstruction (fmap (\m -> M.insert '_' (lookup' m c) m) p) is'
  runInstruction p@(Program ((Rcv c):is) m _) is' = if lookup' m c == 0 then runInstruction (fmap id p) is' else p
  runInstruction p@(Program (s@(Jgz (Register c) i):is) m t) is' = if lookup' m c > 0 then runInstruction (Program (runJgz is' (lookup'' m i) s) m t) is' else runInstruction (fmap id p) is'
  runInstruction p@(Program (s@(Jgz (Number n) i):is) m t) is' = if n > 0 then runInstruction (Program (runJgz is' (lookup'' m i) s) m t) is' else runInstruction (fmap id p) is'

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

  dequeue :: Maybe Queue -> Maybe (Int, Queue)
  dequeue Nothing = Nothing
  dequeue (Just is) = case null is of
    True -> Nothing
    False -> Just (last is, init is)

  parseInput :: String -> Either ParseError Instruction
  parseInput = parse (choice [try parseInstruction, try parseJgz, parseInstruction']) ""

  parseInstruction :: Parser Instruction
  parseInstruction = do
    instruction <- choice [try $ string "mul", try $ string "mod", try $ try $ string "add", string "set"]
    _ <- space
    id <- letter
    _ <- space
    val <- many1 $ choice [try $ char '-', try digit, letter]
    return $ case instruction of
               "mul" -> Mul id (if any isDigit val then Number $ read val else Register $ head val)
               "add" -> Add id (if any isDigit val then Number $ read val else Register $ head val)
               "set" -> Set id (if any isDigit val then Number $ read val else Register $ head val)
               "mod" -> Mod id (if any isDigit val then Number $ read val else Register $ head val)

  parseJgz :: Parser Instruction
  parseJgz = do
    _ <- string "jgz"
    _ <- space
    id <- many1 $ choice [try $ char '-', try digit, letter]
    _ <- space
    val <- many1 $ choice [try $ char '-', try digit, letter]
    return $ Jgz (if any isDigit id then Number $ read id else Register $ head id) (if any isDigit val then Number $ read val else Register $ head val)

  parseInstruction' :: Parser Instruction
  parseInstruction' = do
    instruction <- choice [try $ string "snd", string "rcv"]
    _ <- space
    id <- letter
    return $ case instruction of
      "snd" -> Snd id
      "rcv" -> Rcv id
