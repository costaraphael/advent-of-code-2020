module Day6 (runPart1, runPart2) where

import qualified Data.Set as Set
import Util

input :: String
input = "../inputs/day6.txt"

readInput :: IO [[Set.Set Char]]
readInput = do
  fileContents <- readFile input
  fileContents
    |> lines
    |> partitionBy (== "")
    |> filter (/= [""])
    |> map (map Set.fromList)
    |> pure

runPart1 :: IO ()
runPart1 = do
  groups <- readInput
  groups
    |> map (foldr1 Set.union)
    |> map length
    |> sum
    |> print

runPart2 :: IO ()
runPart2 = do
  groups <- readInput
  groups
    |> map (foldr1 Set.intersection)
    |> map length
    |> sum
    |> print