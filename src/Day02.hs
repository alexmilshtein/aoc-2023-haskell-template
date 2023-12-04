module Day02 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Split (splitOn, splitOneOf)

type Id = Int
type Count = Int
type Subset = (Cubes, Cubes, Cubes)

data Game = Game Id [Subset] deriving (Show, Eq, Ord)
data Cubes = Red Count | Green Count | Blue Count deriving (Show, Eq, Ord)

controlSubset :: (Cubes, Cubes, Cubes)
controlSubset = (Red 12, Green 13, Blue 14)

isPossible :: Game -> Bool
isPossible (Game _ xs) = all (compareWith controlSubset) xs
            where compareWith (Red x', Green y', Blue z') (Red x, Green y, Blue z) = x <= x' && y <= y' && z <= z'
                  compareWith _ _ = False

toCubes :: String -> Cubes
toCubes xs | "red" == cube = Red count
           | "green" == cube = Green count
           | "blue" == cube = Blue count
           | otherwise = error "Incorrect token"
        where count = read . head $ splitted
              cube = last splitted
              splitted = splitOn " " xs

sumGameIds :: [Game] -> Int
sumGameIds = foldr (\(Game i _) res -> res + i) 0

toCubesSubset :: [Cubes] -> (Cubes, Cubes, Cubes)
toCubesSubset = foldr sub (Red 0, Green 0, Blue 0)
    where sub r@(Red _) (x,y,z) = (r <> x, y, z)
          sub g@(Green _) (x,y,z) = (x, g <> y, z)
          sub b@(Blue _) (x,y,z) = (x, y, b <> z)

parseGame :: String -> Game
parseGame line = Game gameId cubes
      where headerAndContents = splitOn ":" line
            gameId = read $ last . splitOn " " $ head headerAndContents
            cubes = toCubesSubset . (toCubes <$>) . splitOn ", " <$> (map tail <$> splitOneOf  ";" $ last headerAndContents)

minimumPossibleSubset :: Game -> Subset
minimumPossibleSubset (Game _ xs) = foldr maximumSubset (Red 0, Green 0, Blue 0) xs
      where maximumSubset (r,g,b) (r',g',b') = (max r r', max g g', max b b')

subsetPower :: Subset -> Int
subsetPower (Red x, Green y, Blue z) = x * y * z
subsetPower _ = error "Incorrect subset"

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let game = parseGame <$> inputLines
  let res = sumGameIds . filter isPossible $ game 
  let res2 = sum $ subsetPower . minimumPossibleSubset <$> game 
  print $ "Part 1: " ++ show res
  print $ "Part 2: "  ++ show res2

instance Semigroup Cubes where
  Red x <> Red y = Red $ x + y
  Green x <> Green y = Green $ x + y
  Blue x <> Blue y = Blue $ x + y
  x <> _ = x