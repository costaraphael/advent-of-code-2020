defmodule Aoc.Day2 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day2.txt"])

  def run_part_1 do
    read_input()
    |> Stream.filter(fn {password, {occurrences_rule, char}} ->
      password
      |> String.graphemes()
      |> Enum.count(&(&1 == char))
      |> Kernel.in(occurrences_rule)
    end)
    |> Enum.count()
    |> IO.puts()
  end

  def run_part_2 do
    read_input()
    |> Stream.filter(fn {password, {occurrences_rule, char}} ->
      [
        String.at(password, occurrences_rule.first - 1),
        String.at(password, occurrences_rule.last - 1)
      ]
      |> Enum.count(&(&1 == char))
      |> Kernel.==(1)
    end)
    |> Enum.count()
    |> IO.puts()
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Stream.map(&String.trim/1)
    |> Stream.reject(&(&1 == ""))
    |> Stream.map(&parse_password_and_rules/1)
  end

  @line_regex ~r/(?<min>\d+)-(?<max>\d+) (?<char>.): (?<password>.+)/

  defp parse_password_and_rules(line) do
    %{"min" => min, "max" => max, "char" => char, "password" => password} =
      Regex.named_captures(@line_regex, line)

    {password, {String.to_integer(min)..String.to_integer(max), char}}
  end
end
