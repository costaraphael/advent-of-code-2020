defmodule Aoc.Day4 do
  import ExValidator

  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day4.txt"])

  def run_part_1 do
    validator =
      map_of(%{
        "byr" => integer(required: true),
        "iyr" => integer(required: true),
        "eyr" => integer(required: true),
        "hgt" => string(required: true),
        "hcl" => string(required: true),
        "ecl" => string(required: true),
        "pid" => string(required: true),
        "cid" => string()
      })

    read_input()
    |> Enum.filter(&valid?(&1, validator))
    |> Enum.count()
  end

  def run_part_2 do
    validator =
      map_of(%{
        "byr" => integer(required: true, min: 1920, max: 2003),
        "iyr" => integer(required: true, min: 2010, max: 2020),
        "eyr" => integer(required: true, min: 2020, max: 2030),
        "hgt" => compose([string(required: true), &valid_height/1]),
        "hcl" => string(required: true, matches: ~r/^#[0-9a-f]{6}$/),
        "ecl" => string(required: true, one_of: ~w[amb blu brn gry grn hzl oth]),
        "pid" => string(required: true, matches: ~r/^\d{9}$/),
        "cid" => string()
      })

    read_input()
    |> Enum.filter(&valid?(&1, validator))
    |> Enum.count()
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Enum.map(&String.trim/1)
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.reject(&(&1 == [""]))
    |> Enum.map(fn lines ->
      lines
      |> Enum.join(" ")
      |> parse_passport()
    end)
  end

  defp valid?(data, validator) do
    match?({:ok, _}, validator.(data))
  end

  defp parse_passport(line) do
    line
    |> String.split(" ")
    |> Map.new(&(&1 |> String.split(":") |> List.to_tuple()))
  end

  defp valid_height(height) do
    Regex.run(~r/^(\d+)(in|cm)$/, height)
    |> case do
      [_, amount, "cm"] -> String.to_integer(amount) in 150..193
      [_, amount, "in"] -> String.to_integer(amount) in 59..76
      _ -> false
    end
    |> if do
      {:ok, height}
    else
      {:error, "invalid height"}
    end
  end
end
