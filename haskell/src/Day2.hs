module Day2
  ( runPart1,
    runPart2,
  )
where

import qualified Data.Either.Unwrap
import qualified Text.Parsec as Parsec
import Util

input :: String
input = "../inputs/day2.txt"

type Password = String

data Rule = Rule Int Int deriving (Show)

data PasswordAndRule = PasswordAndRule Password Rule Char deriving (Show)

parser = do
  min <- Parsec.many1 Parsec.digit
  _ <- Parsec.string "-"
  max <- Parsec.many1 Parsec.digit
  _ <- Parsec.string " "
  char <- Parsec.anyChar
  _ <- Parsec.string ": "
  password <- Parsec.many1 Parsec.anyChar
  return $ PasswordAndRule password (Rule (read min) (read max)) char

parsePasswordAndRule :: String -> PasswordAndRule
parsePasswordAndRule line =
  line
    |> Parsec.parse parser ""
    |> Data.Either.Unwrap.fromRight

readInput :: IO [PasswordAndRule]
readInput = do
  fileContents <- readFile input
  fileContents
    |> lines
    |> map parsePasswordAndRule
    |> pure

solvePart1 :: [PasswordAndRule] -> String
solvePart1 rules = rules |> filter isValid |> length |> show
  where
    isValid (PasswordAndRule password (Rule min max) char) =
      password
        |> filter (== char)
        |> length
        |> (\count -> (count >= min) && (count <= max))

solvePart2 :: [PasswordAndRule] -> String
solvePart2 rules = rules |> filter isValid |> length |> show
  where
    isValid (PasswordAndRule password (Rule min max) char) =
      [password !! (min - 1), password !! (max - 1)]
        |> filter (== char)
        |> length
        |> (== 1)

runPart1 :: IO ()
runPart1 = do
  rules <- readInput
  rules |> solvePart1 |> putStrLn

runPart2 :: IO ()
runPart2 = do
  rules <- readInput
  rules |> solvePart2 |> putStrLn