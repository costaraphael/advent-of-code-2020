defmodule Aoc.Day8 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day8.txt"])

  def run_part_1 do
    {:infinite_loop, acc} = execute(read_input())

    acc
  end

  def run_part_2 do
    instructions = read_input()

    {:finished, acc} =
      instructions
      |> find_jmps_and_nops_indexes()
      |> Stream.map(fn instruction_index ->
        instructions
        |> swap_instruction(instruction_index)
        |> execute()
      end)
      |> Enum.find(&match?({:finished, _}, &1))

    acc
  end

  defp execute(instructions) do
    execute(instructions, 0, 0, MapSet.new())
  end

  defp execute(instructions, current, acc, seen) do
    cond do
      current in seen ->
        {:infinite_loop, acc}

      current >= tuple_size(instructions) ->
        {:finished, acc}

      true ->
        seen = MapSet.put(seen, current)

        case elem(instructions, current) do
          {"nop", _} -> execute(instructions, current + 1, acc, seen)
          {"jmp", arg} -> execute(instructions, current + arg, acc, seen)
          {"acc", arg} -> execute(instructions, current + 1, acc + arg, seen)
        end
    end
  end

  defp find_jmps_and_nops_indexes(instructions) do
    find_jmps_and_nops_indexes(instructions, 0)
  end

  defp find_jmps_and_nops_indexes(instructions, current)
       when current >= tuple_size(instructions) do
    []
  end

  defp find_jmps_and_nops_indexes(instructions, current) do
    case elem(instructions, current) do
      {inst, _} when inst in ~w[jmp nop] ->
        [current | find_jmps_and_nops_indexes(instructions, current + 1)]

      _ ->
        find_jmps_and_nops_indexes(instructions, current + 1)
    end
  end

  defp swap_instruction(instructions, index) do
    case elem(instructions, index) do
      {"jmp", arg} -> put_in(instructions, [Access.elem(index)], {"nop", arg})
      {"nop", arg} -> put_in(instructions, [Access.elem(index)], {"jmp", arg})
    end
  end

  defp read_input do
    @input
    |> File.read!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(fn instruction ->
      [instruction, argument] = String.split(instruction, " ")

      {instruction, String.to_integer(argument)}
    end)
    |> List.to_tuple()
  end
end
