module Advent.Day8 (day8a, day8b, Ast(Ast)) where

  import Data.Either
  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  data Operation = Inc | Dec deriving (Show,Eq)
  data Condition = GT' | LT' | EQ'| GTE' | LTE' | NEQ' | NA deriving (Show, Eq)
  type Number = Int
  type Variable = String
  data Test = Test Variable Condition Number deriving (Show,Eq)
  data Ast = Ast Variable Operation Number Test deriving (Show,Eq)

  day8a :: String -> [Ast]
  day8a = rights . map parseInput . lines

  day8b :: String -> Int
  day8b s = 0

  parseInput :: String -> Either ParseError Ast
  parseInput =  parse (choice [parseStatement]) ""

  parseStatement :: Parser Ast
  parseStatement = do
    l <- many1 letter
    _ <- space
    o <- choice [try $ string "dec", string "inc"]
    _ <- space
    n <- many1 $ choice [try $ char '-', digit]
    _ <- space
    _ <- string "if"
    _ <- space
    l' <- many1 letter
    _ <- space
    o' <- many1 $ oneOf "><=!"
    _ <- space
    n' <- many1 $ choice [try $ char '-', digit]
    return $ Ast l (if o == "dec" then Dec else Inc) (read n) (Test l' (getCondition o') (read n'))

  getCondition:: String -> Condition
  getCondition o = case o of
    ">=" -> GTE'
    "<=" -> LTE'
    ">" -> GT'
    "<" -> LT'
    "==" -> EQ'
    "!=" -> NEQ'
    _ -> NA
