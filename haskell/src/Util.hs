module Util (combinations, (|>), partitionBy) where

(|>) :: a -> (a -> b) -> b
a |> f = f a

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