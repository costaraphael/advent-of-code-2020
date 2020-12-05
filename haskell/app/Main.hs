module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

main :: IO ()
main = do
  _ <- Day1.runPart1
  _ <- Day1.runPart2
  _ <- Day2.runPart1
  _ <- Day2.runPart2
  _ <- Day3.runPart1
  _ <- Day3.runPart2
  _ <- Day4.runPart1
  _ <- Day4.runPart2
  pure ()
