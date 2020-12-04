module Day1
  ( runPart1,
    runPart2,
  )
where

import Util

input :: String
input = "../inputs/day1.txt"

readInput :: IO [Integer]
readInput =
  do
    fileContents <- readFile input
    fileContents
      |> lines
      |> map read
      |> pure

solvePart1 :: [Integer] -> String
solvePart1 numbers =
  numbers
    |> combinations 2
    |> filter (\comb -> sum comb == 2020)
    |> head
    |> product
    |> show

solvePart2 :: [Integer] -> String
solvePart2 numbers =
  numbers
    |> combinations 3
    |> filter (\comb -> sum comb == 2020)
    |> head
    |> product
    |> show

runPart1 :: IO ()
runPart1 = do
  numbers <- readInput
  putStrLn $ solvePart1 numbers

runPart2 :: IO ()
runPart2 = do
  numbers <- readInput
  putStrLn $ solvePart2 numbers