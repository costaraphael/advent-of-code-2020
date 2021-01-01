defmodule Aoc.Day14 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day14.txt"])

  def run_part_1 do
    state = %{mask: nil, memory: %{}}

    read_input()
    |> Enum.reduce(state, &execute_instruction_ver_1/2)
    |> Map.fetch!(:memory)
    |> Map.values()
    |> Enum.sum()
  end

  def run_part_2 do
    state = %{mask: nil, memory: %{}}

    read_input()
    |> Enum.reduce(state, &execute_instruction_ver_2/2)
    |> Map.fetch!(:memory)
    |> Map.values()
    |> Enum.sum()
  end

  defp read_input do
    @input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_instruction/1)
  end

  defp parse_instruction("mask = " <> mask), do: {:mask, String.graphemes(mask)}

  defp parse_instruction("mem[" <> inst) do
    [addr, value] = String.split(inst, "] = ")

    {:init, String.to_integer(addr), String.to_integer(value)}
  end

  defp execute_instruction_ver_1({:mask, mask}, state) do
    %{state | mask: mask}
  end

  defp execute_instruction_ver_1({:init, addr, value}, state) do
    masked_value = mask_value(value, state.mask)

    %{state | memory: Map.put(state.memory, addr, masked_value)}
  end

  defp execute_instruction_ver_2({:mask, mask}, state) do
    %{state | mask: mask}
  end

  defp execute_instruction_ver_2({:init, addr, value}, state) do
    addr
    |> mask_addr(state.mask)
    |> Enum.reduce(state, &%{&2 | memory: Map.put(&2.memory, &1, value)})
  end

  defp mask_value(value, mask) do
    value
    |> Integer.to_string(2)
    |> String.pad_leading(36, "0")
    |> String.graphemes()
    |> Enum.zip(mask)
    |> Enum.map(fn
      {v, "X"} -> v
      {_, m} -> m
    end)
    |> Enum.join()
    |> String.to_integer(2)
  end

  defp mask_addr(addr, mask) do
    addr
    |> Integer.to_string(2)
    |> String.pad_leading(36, "0")
    |> String.graphemes()
    |> do_mask_addr(mask)
    |> Enum.map(fn masked_addr ->
      masked_addr
      |> Enum.join()
      |> String.to_integer(2)
    end)
  end

  defp do_mask_addr([], []), do: [[]]

  defp do_mask_addr([addr_head | addr_tail], ["0" | mask_tail]) do
    addr_tail
    |> do_mask_addr(mask_tail)
    |> Enum.map(&[addr_head | &1])
  end

  defp do_mask_addr([_ | addr_tail], ["1" | mask_tail]) do
    addr_tail
    |> do_mask_addr(mask_tail)
    |> Enum.map(&["1" | &1])
  end

  defp do_mask_addr([_ | addr_tail], ["X" | mask_tail]) do
    addr_tail
    |> do_mask_addr(mask_tail)
    |> Enum.flat_map(&[["0" | &1], ["1" | &1]])
  end
end
