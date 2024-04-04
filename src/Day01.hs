module Day01 where

import Data.Char (isDigit)
import Paths_aoc2023 (getDataFileName)

convertDigitWords :: String -> String
convertDigitWords = go []
  where
    go result [] = result
    go result xs = case xs of
      ('o' : 'n' : 'e' : _) -> go (result ++ "1") (drop 2 xs)
      ('t' : 'w' : 'o' : _) -> go (result ++ "2") (drop 2 xs)
      ('t' : 'h' : 'r' : 'e' : 'e' : _) -> go (result ++ "3") (drop 4 xs)
      ('f' : 'o' : 'u' : 'r' : ys) -> go (result ++ "4") ys
      ('f' : 'i' : 'v' : 'e' : _) -> go (result ++ "5") (drop 3 xs)
      ('s' : 'i' : 'x' : ys) -> go (result ++ "6") ys
      ('s' : 'e' : 'v' : 'e' : 'n' : _) -> go (result ++ "7") (drop 4 xs)
      ('e' : 'i' : 'g' : 'h' : 't' : _) -> go (result ++ "8") (drop 4 xs)
      ('n' : 'i' : 'n' : 'e' : _) -> go (result ++ "9") (drop 3 xs)
      (y : ys) -> go (result ++ [y]) ys

filterDigits :: String -> String
filterDigits = filter isDigit

firstAndLast :: String -> Int
firstAndLast xs = read [head xs, last xs]

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)

  let res = sum $ firstAndLast . filterDigits <$> inputLines
  putStrLn $ mconcat ["Result for part 1: ", show res]

  let res2 = sum $ firstAndLast . filterDigits . convertDigitWords <$> inputLines
  putStrLn $ mconcat ["Result: for part 2: ", show res2]
