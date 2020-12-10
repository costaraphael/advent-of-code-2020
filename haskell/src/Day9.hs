module Day9 (runPart1, runPart2) where

import Util

input :: String
input = "../inputs/day9.txt"

readInput :: IO [Int]
readInput = do
  fileContents <- readFile input
  fileContents
    |> lines
    |> map read
    |> pure

findBadNumber :: (Eq a, Num a) => [a] -> a
findBadNumber numbers =
  let (preamble, rest) = splitAt 25 numbers
   in helper preamble rest
  where
    helper previous (n : ns) =
      if previous |> combinations 2 |> any (\[a, b] -> a + b == n)
        then helper (previous ++ [n]) ns
        else n

runPart1 :: IO ()
runPart1 = readInput >>= print . findBadNumber

findBadNumberSum :: (Ord a, Num a) => [a] -> a -> [a]
findBadNumberSum numbers badNumber =
  case helper numbers 0 [] of
    Nothing -> findBadNumberSum (tail numbers) badNumber
    Just seq -> seq
  where
    helper (n : ns) acc seq
      | acc > badNumber = Nothing
      | acc == badNumber && length seq == 1 = Nothing
      | acc == badNumber = Just seq
      | otherwise = helper ns (acc + n) (n : seq)

runPart2 :: IO ()
runPart2 = do
  numbers <- readInput
  numbers
    |> findBadNumber
    |> findBadNumberSum numbers
    |> sequence [minimum, maximum]
    |> sum
    |> print