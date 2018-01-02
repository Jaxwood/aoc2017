module Advent.Day16 (day16a, day16b) where

    import Data.List (sortBy)
    import Data.Foldable (toList)
    import qualified Data.Sequence as M
    import qualified Data.Text as T
    import qualified Data.Map.Strict as Map
  
    type Program = M.Seq Char
    type Memory = Map.Map String Program
    data Instruction = Spin Int | Exchange Int Int | Partner Char Char deriving (Show,Eq)
  
    day16a :: String -> String -> String
    day16a i s = unpack $ dance (instructions i) (pack s)
  
    day16b :: String -> String -> Int -> String
    day16b i s t = unpack $ iterate' Map.empty (instructions i) t (pack s)
  
    iterate' :: Memory -> [Instruction] -> Int -> Program -> Program
    iterate' m i 0 p = p
    iterate' m i t p = if Map.member (unpack p) m then iterate' m i (pred t) $ (Map.!) m (unpack p) else let a = dance i p
                                                                                                             m' = Map.insert (unpack p) a m
                                                                                                         in iterate' m' i (pred t) a
  
    dance :: [Instruction] -> Program -> Program
    dance is p = foldr follow p is
  
    pack :: String -> Program
    pack = M.fromList
  
    unpack :: Program -> String
    unpack = toList
  
    follow :: Instruction -> Program -> Program
    follow (Spin x) acc = let ln = M.length acc 
                              a = M.take (abs $ ln-x) acc
                              b = M.drop (abs $ ln-x) acc
                          in b M.>< a
    follow (Exchange a b) acc = let (Just a') = M.lookup a acc
                                    (Just b') = M.lookup b acc
                                in M.update b a' (M.update a b' acc)
    follow (Partner a b) acc = let (Just a') = M.elemIndexL a acc
                                   (Just b') = M.elemIndexL b acc
                               in M.update a' b (M.update b' a acc)
  
    instructions :: String -> [Instruction]
    instructions s = reverse $ map (instruction . T.unpack) $ concatMap (T.split (==',') . T.pack) $ lines s
  
    instruction :: String -> Instruction
    instruction ('s':xs) = Spin (read xs)
    instruction ('x':xs) = let x = map (read . T.unpack) $ T.split (=='/') $ T.pack xs
                           in Exchange (head x) (last x)
    instruction ('p':xs) = let x = map T.unpack $ T.split (=='/') $ T.pack xs
                           in Partner (head $ head x) (head $ last x)                                                                                                                                                                                               

                           