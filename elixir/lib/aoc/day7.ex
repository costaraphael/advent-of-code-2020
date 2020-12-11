defmodule Aoc.Day7 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day7.txt"])

  def run_part_1 do
    read_input()
    |> possible_containers("shiny gold")
    |> Enum.uniq()
    |> Enum.count()
  end

  def run_part_2 do
    inner_bags_amount(read_input(), "shiny gold")
  end

  defp possible_containers(map, color) do
    Stream.flat_map(map, fn {container_color, inner_map} ->
      if Map.has_key?(inner_map, color) do
        Stream.concat([container_color], possible_containers(map, container_color))
      else
        []
      end
    end)
  end

  defp inner_bags_amount(map, color) do
    map
    |> Map.get(color, %{})
    |> Enum.map(fn {inner_color, amount} ->
      amount + amount * inner_bags_amount(map, inner_color)
    end)
    |> Enum.sum()
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Map.new(fn line ->
      line_regex = ~r/(?:^|(\d) )(\w+ \w+) bags?/

      [[_, _, outer_color] | contents] = Regex.scan(line_regex, String.trim(line))

      {outer_color,
       Map.new(contents, fn [_, amount, color] -> {color, String.to_integer(amount)} end)}
    end)
  end
end
