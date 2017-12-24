module Advent.Day8 (day8a, day8b) where

  import Data.Either
  import Data.List
  import qualified Data.Map.Strict as M
  import Text.Parsec
  import Text.Parsec.String

  data Operation = Inc | Dec deriving (Show,Eq)
  data Condition = GT' | LT' | EQ'| GTE' | LTE' | NEQ' | NA deriving (Show, Eq)
  type Number = Int
  type Variable = String
  data Test = Test Variable Condition Number deriving (Show,Eq)
  data Ast = Consequent Variable Operation Number Test deriving (Show,Eq)

  day8a :: String -> Int
  day8a = largest . foldl interpretate M.empty . rights . map parseInput . lines

  day8b :: String -> Int
  day8b s = 0

  largest :: M.Map String Int -> Int
  largest m = foldl' (\acc v -> max acc v) 0 m

  interpretate :: M.Map String Int -> Ast -> M.Map String Int
  interpretate m (Consequent v o n (Test v' c n')) =
    if condition c v' n' m then update v o n m else m

  update :: Variable -> Operation -> Number -> M.Map String Int -> M.Map String Int
  update v o n m = 
    let v' = M.lookup v m
    in case v' of
      Nothing -> M.insert v (operation 0 n o) m
      Just v'' -> M.insert v (operation v'' n o) m

  operation :: Number -> Number -> Operation -> Number
  operation n n' o =
    case o of 
      Inc -> n + n'
      Dec -> n - n'
        
  condition :: Condition -> Variable -> Number -> M.Map String Int -> Bool
  condition c v n m =
    let v' = M.lookup v m
    in case v' of
      Nothing -> logic c 0 n
      Just v'' -> logic c v'' n

  logic :: Condition -> Number -> Number -> Bool
  logic c n'' n =
    case c of
      GT' -> n'' > n
      LT' -> n'' < n
      EQ' -> n'' == n
      GTE' -> n'' >= n
      LTE' -> n'' <= n
      NEQ' -> n'' /= n
      NA -> False

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
    return $ Consequent l (if o == "dec" then Dec else Inc) (read n) (Test l' (getCondition o') (read n'))

  getCondition:: String -> Condition
  getCondition o = case o of
    ">=" -> GTE'
    "<=" -> LTE'
    ">" -> GT'
    "<" -> LT'
    "==" -> EQ'
    "!=" -> NEQ'
    _ -> NA
