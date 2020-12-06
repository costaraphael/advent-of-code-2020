module Day5 (runPart1, runPart2) where

import Data.List
import Data.List.Split
import Numeric
import Util

input :: String
input = "../inputs/day5.txt"

getSeatId :: String -> Int
getSeatId seat = fst $ head $ readInt 2 (const True) instructionToBinary seat
  where
    instructionToBinary char
      | char `elem` "FL" = 0
      | otherwise = 1

readInput :: IO [Int]
readInput = do
  fileContents <- readFile input
  fileContents
    |> lines
    |> map getSeatId
    |> pure

runPart1 :: IO ()
runPart1 = do
  seats <- readInput
  seats
    |> maximum
    |> show
    |> putStrLn

runPart2 :: IO ()
runPart2 = do
  seats <- readInput
  seats
    |> sort
    |> divvy 2 1
    |> filter (\[a, b] -> a == b - 2)
    |> map (\[a, _] -> a + 1)
    |> head
    |> print