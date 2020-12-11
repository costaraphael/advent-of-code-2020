defmodule Aoc.Day10 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day10.txt"])

  def run_part_1 do
    result =
      [0 | read_input()]
      |> Stream.chunk_every(2, 1, :discard)
      |> Stream.map(fn [a, b] -> b - a end)
      |> Enum.frequencies()

    Map.get(result, 1, 0) * (Map.get(result, 3, 0) + 1)
  end

  def run_part_2 do
    [0 | read_input()]
    |> possible_sequences()
    |> Enum.reverse()
    |> Enum.reduce({1, 0, 0}, fn
      1, {v1, v2, _v3} -> {v1, v1, v2}
      2, {v1, v2, _v3} -> {v1 + v2, v1, v2}
      3, {v1, v2, v3} -> {v1 + v2 + v3, v1, v2}
    end)
    |> elem(0)
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Stream.map(&(&1 |> String.trim() |> String.to_integer()))
    |> Enum.sort()
  end

  defp possible_sequences([_]), do: [1]

  defp possible_sequences([n | adapters]) do
    [possible_next_adapters(adapters, n) | possible_sequences(adapters)]
  end

  defp possible_next_adapters(adapters, previous) do
    adapters
    |> Enum.take_while(&(&1 - previous <= 3))
    |> Enum.count()
  end
end
