module Day7 (runPart1, runPart2) where

import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Util

input :: String
input = "../inputs/day7.txt"

type BagRules = Map.Map String (Map.Map String Int)

parsePart :: String -> (String, Int)
parsePart part =
  let (amount : color) = splitOn " " part
   in (color |> takeWhile (\w -> w /= "bag" && w /= "bags") |> List.intersperse " " |> concat, read amount)

parseLine :: String -> (String, Map.Map String Int)
parseLine line =
  let [mainColor, parts] = splitOn " bags contain " line
   in if parts == "no other bags"
        then (mainColor, Map.empty)
        else (mainColor, parts |> splitOn ", " |> map parsePart |> Map.fromList)

readInput :: IO BagRules
readInput = do
  fileContents <- readFile input
  fileContents
    |> splitOn ".\n"
    |> filter (/= "")
    |> map parseLine
    |> Map.fromList
    |> pure

containerBags :: String -> BagRules -> [String]
containerBags color rules =
  rules
    |> Map.toList
    |> map
      ( \(containerColor, innerMap) ->
          if Map.member color innerMap
            then containerColor : containerBags containerColor rules
            else []
      )
    |> concat

runPart1 :: IO ()
runPart1 = do
  rules <- readInput
  rules
    |> containerBags "shiny gold"
    |> Set.fromList
    |> length
    |> print

innerBagAmount :: String -> BagRules -> Int
innerBagAmount color rules =
  case Map.lookup color rules of
    Nothing -> 0
    Just innerMap ->
      innerMap
        |> Map.toList
        |> map (\(innerColor, amount) -> amount + amount * innerBagAmount innerColor rules)
        |> sum

runPart2 :: IO ()
runPart2 = do
  rules <- readInput
  rules
    |> innerBagAmount "shiny gold"
    |> print