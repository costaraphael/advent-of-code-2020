module Util (combinations, (|>), partitionBy, parse) where

import qualified Text.Parsec as Parsec

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixl 9 |>

combinations :: Integer -> [a] -> [[a]]
combinations _ [] = []
combinations 1 list = map (\n -> [n]) list
combinations size (head : tail) =
  map (head :) (combinations (size - 1) tail)
    ++ combinations size tail

partitionBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
partitionBy _ [] = []
partitionBy pred (x : xs) = (x : sameGroupElements) : partitionBy pred rest
  where
    (sameGroupElements, rest) = span (\n -> (pred n) == initialValue) xs
    initialValue = pred x

parse :: Parsec.Parsec String () result -> String -> result
parse parser line =
  case Parsec.parse parser "" line of
    Right value -> value
    Left e -> e |> show |> error