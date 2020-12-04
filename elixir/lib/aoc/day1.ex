defmodule Aoc.Day1 do
  alias Aoc.Util

  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day1.txt"])

  def run_part_1 do
    [a, b] =
      read_input()
      |> Util.combinations(2)
      |> Enum.find(fn [a, b] ->
        a + b == 2020
      end)

    IO.puts(a * b)
  end

  def run_part_2 do
    [a, b, c] =
      read_input()
      |> Util.combinations(3)
      |> Enum.find(fn [a, b, c] ->
        a + b + c == 2020
      end)

    IO.puts(a * b * c)
  end

  defp read_input do
    @input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end
end
