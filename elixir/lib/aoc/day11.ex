defmodule Aoc.Day11 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day11.txt"])

  defmodule SeatMap do
    defstruct [:w, :h, :seats]

    defimpl Inspect do
      def inspect(%SeatMap{} = seat_map, _opts) do
        0..seat_map.h
        |> Enum.map(fn row ->
          0..seat_map.w
          |> Enum.map(fn column ->
            case Map.fetch!(seat_map.seats, {row, column}) do
              :empty -> "L"
              :floor -> "."
              :occupied -> "#"
            end
          end)
          |> Inspect.Algebra.concat()
        end)
        |> Enum.intersperse(Inspect.Algebra.line())
        |> Inspect.Algebra.concat()
      end
    end
  end

  def run_part_1 do
    read_input()
    |> run_until_no_changes(:adjacent, 4)
    |> count_occupied_seats()
  end

  def run_part_2 do
    read_input()
    |> run_until_no_changes(:visibility, 5)
    |> count_occupied_seats()
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Enum.map(&parse_line/1)
    |> parse_map()
  end

  defp parse_line(line) do
    line
    |> String.trim()
    |> String.graphemes()
    |> Enum.map(fn
      "." -> :floor
      "L" -> :empty
    end)
  end

  defp parse_map([row | _] = rows) do
    w = length(row) - 1
    h = length(rows) - 1

    seats =
      rows
      |> Stream.with_index()
      |> Enum.flat_map(fn {row, row_index} ->
        row
        |> Stream.with_index()
        |> Enum.map(fn {seat, column_index} ->
          {{row_index, column_index}, seat}
        end)
      end)
      |> Map.new()

    %SeatMap{w: w, h: h, seats: seats}
  end

  defp run_once(%SeatMap{} = seat_map, neighbors_rule, max_occupied_neighbors) do
    for row <- 0..seat_map.h,
        column <- 0..seat_map.w,
        reduce: seat_map do
      new_seat_map ->
        case Map.fetch!(seat_map.seats, {row, column}) do
          :floor ->
            new_seat_map

          :empty ->
            seat_map
            |> find_neighbors(row, column, neighbors_rule)
            |> Enum.all?(&(Map.fetch!(seat_map.seats, &1) == :empty))
            |> if do
              put_in(new_seat_map.seats[{row, column}], :occupied)
            else
              new_seat_map
            end

          :occupied ->
            seat_map
            |> find_neighbors(row, column, neighbors_rule)
            |> Enum.count(&(Map.fetch!(seat_map.seats, &1) == :occupied))
            |> Kernel.>=(max_occupied_neighbors)
            |> if do
              put_in(new_seat_map.seats[{row, column}], :empty)
            else
              new_seat_map
            end
        end
    end
  end

  defp run_until_no_changes(seat_map, neighbors_rule, max_occupied_neighbors) do
    case run_once(seat_map, neighbors_rule, max_occupied_neighbors) do
      ^seat_map -> seat_map
      new_seat_map -> run_until_no_changes(new_seat_map, neighbors_rule, max_occupied_neighbors)
    end
  end

  defp count_occupied_seats(seat_map) do
    for row <- 0..seat_map.h,
        column <- 0..seat_map.w,
        Map.fetch!(seat_map.seats, {row, column}) == :occupied,
        reduce: 0,
        do: (acc -> acc + 1)
  end

  defp find_neighbors(seat_map, row, column, :adjacent) do
    for {row_delta, column_delta} <- neighbor_deltas(),
        new_row = row + row_delta,
        new_column = column + column_delta,
        Map.get(seat_map.seats, {new_row, new_column}, :floor) != :floor,
        do: {new_row, new_column}
  end

  defp find_neighbors(seat_map, row, column, :visibility) do
    for {row_delta, column_delta} <- neighbor_deltas(),
        neighbor_seat = find_next_seat(seat_map, row, column, row_delta, column_delta),
        not is_nil(neighbor_seat),
        do: neighbor_seat
  end

  defp neighbor_deltas do
    for row <- -1..1,
        column <- -1..1,
        {row, column} != {0, 0},
        do: {row, column}
  end

  defp find_next_seat(seat_map, row, column, row_delta, column_delta) do
    new_row = row + row_delta
    new_column = column + column_delta

    case Map.get(seat_map.seats, {new_row, new_column}, :out_of_bounds) do
      :floor -> find_next_seat(seat_map, new_row, new_column, row_delta, column_delta)
      :out_of_bounds -> nil
      _ -> {new_row, new_column}
    end
  end
end
