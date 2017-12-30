module Advent.Day16 (day16a, day16b, Instruction(Spin, Exchange, Partner)) where

  import Data.List
  import qualified Data.Text as T

  data Instruction = Spin Int | Exchange Int Int | Partner Char Char deriving (Show,Eq)

  day16a :: String -> String -> String
  day16a s i = foldl follow s $ map (instruction . T.unpack) $ concatMap (T.split (==',') . T.pack) $ lines i

  day16b :: String -> String
  day16b s = ""

  follow :: String -> Instruction -> String
  follow acc (Spin x) = let ln = length acc
                        in take ln $ drop (abs $ ln - x) $ cycle acc

  follow acc (Exchange a b) = swap (Just a) (Just b) acc

  follow acc (Partner a b) = let a' = findIndex (==a) acc
                                 b' = findIndex (==b) acc
                             in swap a' b' acc

  swap :: Maybe Int -> Maybe Int -> String -> String
  swap (Just a) (Just b) acc = let a' = acc !! a
                                   b' = acc !! b
                                   acc' = take a acc ++ (b':(drop (succ a) acc))
                               in take b acc' ++ (a':(drop (succ b) acc'))
  swap _ _ acc = acc

  instruction :: String -> Instruction
  instruction ('s':xs) = Spin (read xs)
  instruction ('x':xs) = let x = map (read . T.unpack) $ T.split (=='/') $ T.pack xs
                         in Exchange (head x) (last x)
  instruction ('p':xs) = let x = map T.unpack $ T.split (=='/') $ T.pack xs
                         in Partner (head $ head x) (head $ last x)

