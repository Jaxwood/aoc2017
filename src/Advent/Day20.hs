module Advent.Day20 (day20a, day20b) where

  import Data.List
  import Text.Parsec
  import Text.Parsec.String

  type X = Int
  type Y = Int
  type Z = Int

  data Position = Position X Y Z deriving (Show,Eq)
  data Velocity = Velocity X Y Z deriving (Show,Eq)
  data Acceleration = Acceleration X Y Z deriving (Show,Eq)

  data Particle = Particle Position Velocity Acceleration deriving (Show,Eq)

  instance Ord Particle where
    compare p@(Particle (Position x y z) _ _) p'@(Particle (Position x' y' z') _ _) =
      if collisions p p' then EQ else if sum [x, y, z] > sum [x',y',z'] then GT else LT

  day20a :: String -> Int
  day20a s = fst $ mimimum' $ map (iterate' 10000) $ zip [0..] $ map (right . parseInput) $ lines s

  day20b :: String -> Int
  day20b s = length $ iterate'' 0 $ map (right . parseInput) $ lines s

  mimimum' :: [(Int,Int)] -> (Int,Int)
  mimimum' = foldr1 (\x acc -> if snd x < snd acc then x else acc)

  iterate' :: Int -> (Int, Particle) -> (Int,Int)
  iterate' t (i,p) = last $  map (\x -> (i,manhattan x)) $ take t $ iterate tick p

  iterate'' :: Int -> [Particle] -> [Particle]
  iterate'' 10000 ps = ps
  iterate'' i ps = let ps' = map tick ps
                       ps'' = concat $ filter (\x -> length x == 1) $ groupBy collisions $ sortBy (\p p' -> p `compare` p') ps'
                   in iterate'' (succ i) ps''

  manhattan :: Particle -> Int
  manhattan (Particle (Position x y z) _ _) = (abs x + abs y + abs z)

  collisions :: Particle -> Particle -> Bool
  collisions (Particle (Position x y z) _ _) (Particle (Position x' y' z') _ _) =
    and [x == x', y == y', z == z']

  tick :: Particle -> Particle
  tick (Particle (Position x y z) (Velocity x' y' z') (Acceleration x'' y'' z'')) =
    let v = (x'+x'')
        v' = (y'+y'')
        v'' = (z'+z'')
    in (Particle (Position (x+v) (y+v') (z+v'')) (Velocity v v' v'') (Acceleration x'' y'' z''))

  -- utility

  right :: Either ParseError Particle -> Particle
  right e = case e of
              (Left e) -> error $ show e
              (Right p) -> p

  -- parse

  parseInput :: String -> Either ParseError Particle
  parseInput = parse (choice [parseParticle]) ""

  parseParticle :: Parser Particle
  parseParticle = do
    _ <- string "p=<"
    (a:a':a'':[]) <- sepBy (many1 $ choice [try $ char '-', digit]) $ char ','
    _ <- string ">, v=<"
    (b:b':b'':[]) <- sepBy (many1 $ choice [try $ char '-', digit]) $ char ','
    _ <- string ">, a=<"
    (c:c':c'':[]) <- sepBy (many1 $ choice [try $ char '-', digit]) $ char ','
    _ <- char '>'
    return $ Particle
      (Position (read a) (read a') (read a''))
      (Velocity (read b) (read b') (read b''))
      (Acceleration (read c) (read c') (read c''))
