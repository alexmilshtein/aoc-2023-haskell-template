module Day01 where

import Paths_aoc2023 (getDataFileName)
import Data.Char (isDigit)

filterDigits :: String -> String
filterDigits = filter isDigit

firstAndLast:: String -> Int
firstAndLast [x] = read [x,x]
firstAndLast xs = read [head xs, last xs]

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let res = sum $ firstAndLast . filterDigits <$> inputLines
  print $ mconcat ["Result: ", show res]