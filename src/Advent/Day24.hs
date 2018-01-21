module Advent.Day24 (day24a, day24b) where

  import Text.Parsec
  import Text.Parsec.String

  data Bridge = Bridge Int Int

  day24a :: String -> Int
  day24a s = 0

  day24b :: String -> Int
  day24b s = 0

  -- utility

  right :: Either ParseError Bridge -> Bridge
  right (Left e) = error $ show e
  right (Right r) = r

  -- parse 

  parseInput :: String -> Either ParseError Bridge
  parseInput = parse parseBridge ""

  parseBridge :: Parser Bridge
  parseBridge = do
    str <- many1 digit
    _ <- char '/'
    str' <- many1 digit
    return $ Bridge (read str) (read str')
