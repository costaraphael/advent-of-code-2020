defmodule Aoc.Day5 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day5.txt"])

  def run_part_1 do
    Enum.max(read_input())
  end

  def run_part_2 do
    read_input()
    |> Enum.sort()
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.filter(fn [a, b] -> a == b - 2 end)
    |> Enum.map(fn [a, _] -> a + 1 end)
    |> List.first()
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.map(fn line ->
      line
      |> String.replace(~r/./, fn
        c when c in ~w[F L] -> "0"
        c when c in ~w[B R] -> "1"
      end)
      |> String.to_integer(2)
    end)
  end
end
