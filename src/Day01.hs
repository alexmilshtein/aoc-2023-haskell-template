module Day01 where

import Paths_aoc2023 (getDataFileName)
import Data.Char (isDigit)

convertDigitWords :: String -> String
convertDigitWords = go []
      where go result [] = result
            go result xs = case xs of
                  ('o':'n':'e':ys) -> go (result ++ "1") (take 2 (tail xs) ++ ys)
                  ('t':'w':'o':ys) -> go (result ++ "2") (take 2 (tail xs) ++ ys)
                  ('t':'h':'r':'e':'e':ys) -> go (result ++ "3") (take 4 (tail xs) ++ ys)
                  ('f':'o':'u':'r':ys) ->  go (result ++ "4") (take 3 (tail xs) ++ ys)
                  ('f':'i':'v':'e':ys) -> go (result ++ "5") (take 3 (tail xs) ++ ys)
                  ('s':'i':'x':ys) -> go (result ++ "6") (take 2 (tail xs) ++ ys)
                  ('s':'e':'v':'e':'n':ys) -> go (result ++ "7") (take 4 (tail xs) ++ ys)
                  ('e':'i':'g':'h':'t':ys) -> go (result ++ "8") (take 4 (tail xs) ++ ys)
                  ('n':'i':'n':'e':ys) -> go (result ++ "9") (take 3 (tail xs) ++ ys)
                  (y:ys) -> go (result ++ [y]) ys



filterDigits :: String -> String
filterDigits = filter isDigit

firstAndLast:: String -> Int
firstAndLast xs = read [head xs, last xs]

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)

  let res = sum $ firstAndLast . filterDigits <$> inputLines
  putStrLn $ mconcat ["Result for part 1: ", show res]

  let res2 = sum $ firstAndLast . filterDigits . convertDigitWords <$> inputLines
  putStrLn $ mconcat ["Result: for part 2: ", show res2]