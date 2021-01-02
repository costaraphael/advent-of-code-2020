defmodule Aoc.Day15 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day15.txt"])

  def run_part_1 do
    read_input()
    |> find_number(2020)
  end

  def run_part_2 do
    read_input()
    |> find_number(30_000_000)
  end

  defp find_number(starting_numbers, wanted_position) do
    starting_numbers_amount = length(starting_numbers)

    starting_numbers
    |> List.delete_at(-1)
    |> Enum.with_index()
    |> Map.new(fn {number, index} -> {number, index + 1} end)
    |> do_find_number(
      List.last(starting_numbers),
      starting_numbers_amount,
      wanted_position - starting_numbers_amount
    )
  end

  defp do_find_number(seen_numbers, last_number, current_position, counter)

  defp do_find_number(_, last_number, _, 0), do: last_number

  defp do_find_number(seen_numbers, last_number, current_position, counter) do
    next_number =
      case seen_numbers do
        %{^last_number => last_position} -> current_position - last_position
        _ -> 0
      end

    seen_numbers = Map.put(seen_numbers, last_number, current_position)

    do_find_number(seen_numbers, next_number, current_position + 1, counter - 1)
  end

  defp read_input do
    @input
    |> File.read!()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end
end
