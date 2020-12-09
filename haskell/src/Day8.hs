{-# LANGUAGE LambdaCase #-}

module Day8 (runPart1, runPart2) where

import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Util

input :: String
input = "../inputs/day8.txt"

type Program = Map.Map Int (String, Int)

parseInstruction :: String -> (String, Int)
parseInstruction instruction = do
  let [command, arg] = splitOn " " instruction
   in (command, arg |> dropWhile (== '+') |> read)

readInput :: IO Program
readInput = do
  fileContents <- readFile input
  fileContents
    |> lines
    |> map parseInstruction
    |> zip (iterate (+ 1) 0)
    |> Map.fromList
    |> pure

data ExecutionResult = InfiniteLoop Int | Finished Int deriving (Show)

execute :: Program -> ExecutionResult
execute program = do_execute 0 0 Set.empty program
  where
    do_execute index acc seen program
      | index `elem` seen = InfiniteLoop acc
      | otherwise =
        let newSeen = Set.insert index seen
         in case Map.lookup index program of
              Nothing -> Finished acc
              Just ("nop", _) -> do_execute (index + 1) acc newSeen program
              Just ("acc", arg) -> do_execute (index + 1) (acc + arg) newSeen program
              Just ("jmp", arg) -> do_execute (index + arg) acc newSeen program

runPart1 :: IO ()
runPart1 = do
  program <- readInput
  let InfiniteLoop acc = execute program
  print acc

findJmpsAndNops :: Program -> [Int]
findJmpsAndNops program =
  program
    |> Map.toList
    |> filter (\(_, (command, _)) -> command == "jmp" || command == "nop")
    |> map fst

swapInstruction :: Program -> Int -> Program
swapInstruction program idx =
  case Map.lookup idx program of
    Just ("nop", arg) -> Map.insert idx ("jmp", arg) program
    Just ("jmp", arg) -> Map.insert idx ("nop", arg) program

runPart2 :: IO ()
runPart2 = do
  program <- readInput
  let Finished acc =
        program
          |> findJmpsAndNops
          |> map (execute . swapInstruction program)
          |> filter
            ( \case
                (Finished _) -> True
                (InfiniteLoop _) -> False
            )
          |> head
  print acc