defmodule Aoc.Day12 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day12.txt"])

  def run_part_1 do
    ship = %{direction: "E", coordinates: {0, 0}}

    read_input()
    |> Enum.reduce(ship, fn
      {"F", units}, ship -> update_in(ship.coordinates, &move(&1, ship.direction, units))
      {"R", degrees}, ship -> rotate_ship(ship, degrees)
      {"L", degrees}, ship -> rotate_ship(ship, -degrees)
      {direction, units}, ship -> update_in(ship.coordinates, &move(&1, direction, units))
    end)
    |> calculate_manhattan_distance()
  end

  def run_part_2 do
    ship = %{direction: "E", coordinates: {0, 0}}
    waypoint = {10, 1}

    read_input()
    |> Enum.reduce({ship, waypoint}, fn
      {"F", times}, {ship, waypoint} ->
        {update_in(ship.coordinates, &move_to_waypoint(&1, waypoint, times)), waypoint}

      {"R", degrees}, {ship, waypoint} ->
        {ship, rotate_waypoint(waypoint, degrees)}

      {"L", degrees}, {ship, waypoint} ->
        {ship, rotate_waypoint(waypoint, -degrees)}

      {direction, units}, {ship, waypoint} ->
        {ship, move(waypoint, direction, units)}
    end)
    |> elem(0)
    |> calculate_manhattan_distance()
  end

  defp read_input do
    @input
    |> File.stream!()
    |> Enum.map(fn line ->
      {command, argument} = line |> String.trim() |> String.split_at(1)
      {command, String.to_integer(argument)}
    end)
  end

  defp calculate_manhattan_distance(%{coordinates: {e, n}}), do: abs(e) + abs(n)

  defp move({e, n}, "E", units), do: {e + units, n}
  defp move({e, n}, "S", units), do: {e, n - units}
  defp move({e, n}, "W", units), do: {e - units, n}
  defp move({e, n}, "N", units), do: {e, n + units}

  @directions ~w[E S W N]

  defp rotate_ship(ship, degrees) do
    rotations =
      degrees
      |> abs()
      |> div(90)
      |> rem(4)

    new_direction =
      if degrees >= 0 do
        @directions
      else
        Enum.reverse(@directions)
      end
      |> Stream.cycle()
      |> Stream.drop_while(&(&1 != ship.direction))
      |> Stream.drop(rotations)
      |> Enum.at(0)

    %{ship | direction: new_direction}
  end

  defp rotate_waypoint({delta_x, delta_y}, degrees) when degrees in [90, -270],
    do: {delta_y, -delta_x}

  defp rotate_waypoint({delta_x, delta_y}, degrees) when degrees in [180, -180],
    do: {-delta_x, -delta_y}

  defp rotate_waypoint({delta_x, delta_y}, degrees) when degrees in [270, -90],
    do: {-delta_y, delta_x}

  defp move_to_waypoint({e, n}, {delta_e, delta_n}, times),
    do: {e + delta_e * times, n + delta_n * times}
end
