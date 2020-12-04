module Util (combinations, (|>)) where

(|>) :: a -> (a -> b) -> b
a |> f = f a

infixl 9 |>

combinations :: Integer -> [a] -> [[a]]
combinations _ [] = []
combinations 1 list = map (\n -> [n]) list
combinations size (head : tail) =
  map (head :) (combinations (size - 1) tail)
    ++ combinations size tail