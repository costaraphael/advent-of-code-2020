defmodule Aoc.Day6 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day6.txt"])

  def run_part_1 do
    read_input()
    |> Stream.map(fn group ->
      group
      |> Enum.reduce(&MapSet.union/2)
      |> Enum.count()
    end)
    |> Enum.sum()
  end

  def run_part_2 do
    read_input()
    |> Stream.map(fn group ->
      group
      |> Enum.reduce(&MapSet.intersection/2)
      |> Enum.count()
    end)
    |> Enum.sum()
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.chunk_by(&(&1 == ""))
    |> Stream.reject(&(&1 == [""]))
    |> Stream.map(fn group ->
      Enum.map(group, &(&1 |> String.graphemes() |> MapSet.new()))
    end)
  end
end
