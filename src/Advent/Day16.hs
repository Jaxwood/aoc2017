module Advent.Day16 (day16a, day16b, Instruction(Spin, Exchange, Partner)) where

  import qualified Data.Text as T

  data Instruction = Spin Int | Exchange Int Int | Partner String String deriving (Show,Eq)

  day16a :: String -> String -> [Instruction]
  day16a s i = map (instruction . T.unpack) $ concatMap (T.split (==',') . T.pack) $ lines i

  day16b :: String -> String
  day16b s = ""

  instruction :: String -> Instruction
  instruction ('s':xs) = Spin (read xs)
  instruction ('x':xs) = let x = map (read . T.unpack) $ T.split (=='/') $ T.pack xs
                         in Exchange (head x) (last x)
  instruction ('p':xs) = let x = map T.unpack $ T.split (=='/') $ T.pack xs
                         in Partner (head x) (last x)

