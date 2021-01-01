defmodule Aoc.Day13 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day13.txt"])

  def run_part_1 do
    {earliest_timestamp, buses} = read_input()

    {minutes_to_wait, bus_id} =
      buses
      |> Enum.reject(&(&1 == :x))
      |> Enum.map(&{&1 - rem(earliest_timestamp, &1), &1})
      |> Enum.min()

    minutes_to_wait * bus_id
  end

  def run_part_2 do
    {_, buses} = read_input()

    buses
    |> Enum.with_index()
    |> Enum.reject(&match?({:x, _}, &1))
    |> Enum.map(fn {m, index} -> {m - index, m} end)
    |> chinese_remainder_theorem()
    |> elem(0)
  end

  defp chinese_remainder_theorem(pairs) do
    ms = Enum.map(pairs, fn {_, m} -> m end)

    final_modulo = Enum.reduce(ms, &Kernel.*/2)

    as = Enum.map(pairs, fn {a, _} -> a end)
    ns = Enum.map(ms, &div(final_modulo, &1))

    inverses =
      [ns, ms]
      |> Enum.zip()
      |> Enum.map(fn {a, m} -> modular_multiplicative_inverse(a, m) end)

    offset =
      [as, ns, inverses]
      |> Enum.zip()
      |> Enum.map(fn {a, m, inverse} -> a * m * inverse end)
      |> Enum.sum()
      |> Integer.mod(final_modulo)

    {offset, final_modulo}
  end

  defp modular_multiplicative_inverse(a, m) do
    a = Integer.mod(a, m)

    Enum.find(1..m, &(Integer.mod(a * &1, m) == 1))
  end

  defp read_input do
    [earliest_timestamp, buses] =
      @input
      |> File.read!()
      |> String.trim()
      |> String.split("\n")

    parsed_buses =
      buses
      |> String.split(",")
      |> Enum.map(fn
        "x" -> :x
        n -> String.to_integer(n)
      end)

    {String.to_integer(earliest_timestamp), parsed_buses}
  end
end
