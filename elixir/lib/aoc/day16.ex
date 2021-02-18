defmodule Aoc.Day16 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day16.txt"])

  def run_part_1 do
    input = read_input()

    input.nearby_tickets
    |> List.flatten()
    |> Enum.reject(fn value ->
      Enum.any?(input.rules, fn {_field, ranges} ->
        Enum.any?(ranges, &(value in &1))
      end)
    end)
    |> Enum.sum()
  end

  def run_part_2 do
    input = read_input()

    correct_fields =
      input.nearby_tickets
      |> Enum.filter(fn ticket ->
        Enum.all?(ticket, fn value ->
          Enum.any?(input.rules, fn {_field, ranges} ->
            Enum.any?(ranges, &(value in &1))
          end)
        end)
      end)
      |> find_field_possibilities(input.rules)
      |> discover_correct_fields()

    correct_fields
    |> Enum.zip(input.my_ticket)
    |> Enum.filter(fn {field, _value} ->
      field
      |> to_string()
      |> String.starts_with?("departure")
    end)
    |> Keyword.values()
    |> Enum.reduce(&Kernel.*/2)
  end

  defp read_input do
    {rules, [_, my_ticket, _ | nearby_tickets]} =
      @input
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.split_while(&(&1 != "your ticket:"))

    %{
      rules: parse_rules(rules),
      my_ticket: parse_ticket(my_ticket),
      nearby_tickets: Enum.map(nearby_tickets, &parse_ticket/1)
    }
  end

  defp find_field_possibilities(nearby_tickets, rules) do
    initial_field_possibilities = List.duplicate(rules, length(rules))

    Enum.reduce(nearby_tickets, initial_field_possibilities, fn ticket, field_possibilities ->
      field_possibilities
      |> Enum.zip(ticket)
      |> Enum.map(fn {possible_fields, value} ->
        possible_fields
        |> Enum.filter(fn {_field, ranges} ->
          Enum.any?(ranges, &(value in &1))
        end)
      end)
    end)
  end

  defp discover_correct_fields(field_possibilities) do
    if Enum.all?(field_possibilities, &(length(&1) == 1)) do
      field_possibilities
      |> List.flatten()
      |> Enum.map(fn {field, _rules} -> field end)
    else
      fields_with_one_possibility =
        field_possibilities
        |> Enum.filter(&(length(&1) == 1))
        |> List.flatten()
        |> Keyword.keys()

      field_possibilities
      |> Enum.map(fn
        [_] = possible_fields -> possible_fields
        possible_fields -> Keyword.drop(possible_fields, fields_with_one_possibility)
      end)
      |> discover_correct_fields()
    end
  end

  defp parse_rules(rules) do
    rules
    |> Enum.map(fn rule ->
      [field, ranges] = String.split(rule, ": ")

      parsed_ranges =
        ranges
        |> String.split(" or ")
        |> Enum.map(fn range ->
          [lower_bound, upper_bound] =
            range |> String.split("-") |> Enum.map(&String.to_integer/1)

          lower_bound..upper_bound
        end)

      {String.to_atom(field), parsed_ranges}
    end)
  end

  defp parse_ticket(ticket) do
    ticket
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end
end
