module Day7 (runPart1, runPart2) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec hiding (parse)
import Util

input :: String
input = "../inputs/day7.txt"

type BagRules = Map.Map String (Map.Map String Int)

lineParser :: Parsec String u (String, Map.Map String Int)
lineParser = do
  containerColor <- manyTill anyChar (try (string " bags contain "))
  items <- emptyItemsParser <|> sepBy itemParser (string ", ")
  return (containerColor, Map.fromList items)
  where
    emptyItemsParser = string "no other bags" >> return []
    itemParser = do
      amount <- many1 digit
      string " "
      color <- manyTill anyChar (try (string " bags") <|> try (string " bag"))
      return (color, read amount)

readInput :: IO BagRules
readInput = do
  fileContents <- readFile input
  fileContents
    |> splitOn ".\n"
    |> filter (/= "")
    |> map (parse lineParser)
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