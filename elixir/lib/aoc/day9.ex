defmodule Aoc.Day9 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day9.txt"])

  alias Aoc.Util

  def run_part_1 do
    {preamble, numbers} = Enum.split(read_input(), 25)

    find_non_sum_of_previous(numbers, preamble)
  end

  def run_part_2 do
    original_numbers = read_input()

    {preamble, numbers} = Enum.split(original_numbers, 25)

    sum_numbers =
      numbers
      |> find_non_sum_of_previous(preamble)
      |> find_numbers_that_add_up_to(original_numbers)

    {min, max} = Enum.min_max(sum_numbers)

    min + max
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Stream.map(&(&1 |> String.trim() |> String.to_integer()))
    |> Enum.to_list()
  end

  defp find_non_sum_of_previous([n | ns], [_ | previous_tail] = previous) do
    previous
    |> Util.combinations(2)
    |> Enum.any?(fn [a, b] -> a + b == n end)
    |> if do
      find_non_sum_of_previous(ns, previous_tail ++ [n])
    else
      n
    end
  end

  defp find_numbers_that_add_up_to(target, [_ | numbers_tail] = numbers) do
    case do_find_numbers_that_add_up_to(target, numbers, 0, []) do
      :not_found -> find_numbers_that_add_up_to(target, numbers_tail)
      {:found, numbers} -> numbers
    end
  end

  defp do_find_numbers_that_add_up_to(_target, [], _acc, _seq), do: :not_found

  defp do_find_numbers_that_add_up_to(target, [n | ns], acc, seq) when acc < target,
    do: do_find_numbers_that_add_up_to(target, ns, acc + n, [n | seq])

  defp do_find_numbers_that_add_up_to(target, _list, acc, _seq) when acc > target,
    do: :not_found

  defp do_find_numbers_that_add_up_to(target, _list, acc, [_]) when acc == target,
    do: :not_found

  defp do_find_numbers_that_add_up_to(target, _list, acc, seq) when acc == target,
    do: {:found, seq}
end
