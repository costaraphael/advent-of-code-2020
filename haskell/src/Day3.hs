{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Day3
  ( runPart1,
    runPart2,
  )
where

import Data.String.Interpolate
import Util

input :: String
input = "../inputs/day3.txt"

data TreeMapItem = Empty | Tree deriving (Show, Eq)

data TreeMap = TreeMap {contents :: [[TreeMapItem]], height :: Int, width :: Int} deriving (Show)

readInput :: IO TreeMap
readInput = do
  fileContents <- readFile input
  let contents =
        fileContents
          |> lines
          |> map (map parseChar)
        where
          parseChar '.' = Empty
          parseChar '#' = Tree
          parseChar char = error [i|invalid character '#{char}' found|]

  let height = length contents
  let width = contents |> head |> length
  pure $ TreeMap {contents, height, width}

treeMapGet :: Int -> Int -> TreeMap -> TreeMapItem
treeMapGet x y treeMap = treeMap |> contents |> (!! y) |> (!! actualX)
  where
    actualX = x `rem` width treeMap

traverseTreeMap :: Int -> Int -> TreeMap -> [TreeMapItem]
traverseTreeMap x_movement y_movement treeMap = doTraverse 0 0
  where
    doTraverse x y =
      if outOfBounds
        then []
        else treeMapGet x y treeMap : doTraverse (x + x_movement) (y + y_movement)
      where
        outOfBounds = y >= height treeMap

solvePart1 :: TreeMap -> String
solvePart1 treeMap =
  treeMap
    |> traverseTreeMap 3 1
    |> filter (== Tree)
    |> length
    |> show

solvePart2 :: TreeMap -> String
solvePart2 treeMap =
  [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    |> map
      ( \(x_movement, y_movement) ->
          treeMap
            |> traverseTreeMap x_movement y_movement
            |> filter (== Tree)
            |> length
      )
    |> product
    |> show

runPart1 :: IO ()
runPart1 = do
  treeMap <- readInput
  putStrLn $ solvePart1 treeMap

runPart2 :: IO ()
runPart2 = do
  treeMap <- readInput
  putStrLn $ solvePart2 treeMap