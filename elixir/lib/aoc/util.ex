defmodule Aoc.Util do
  @moduledoc """
  Utility functions
  """

  @spec combinations(list :: [a], size :: pos_integer()) :: [[a]] when a: term
  def combinations(list, size)

  def combinations([], _size), do: []

  def combinations(list, 1) do
    Enum.map(list, &[&1])
  end

  def combinations([head | tail], size) when is_integer(size) and size > 0 do
    tail
    |> combinations(size - 1)
    |> Enum.map(&[head | &1])
    |> Enum.concat(combinations(tail, size))
  end
end
