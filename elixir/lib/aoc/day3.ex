defmodule Aoc.Day3 do
  @input Path.join([__DIR__, "..", "..", "..", "inputs", "day3.txt"])

  defmodule TreeMap do
    defstruct [:map, :width, :heigth]

    def parse(string) do
      map =
        string
        |> String.split("\n", trim: true)
        |> Enum.map(fn line ->
          line
          |> String.graphemes()
          |> Enum.map(fn
            "." -> :empty
            "#" -> :tree
          end)
          |> List.to_tuple()
        end)
        |> List.to_tuple()

      width = map |> elem(0) |> tuple_size()
      height = tuple_size(map)

      %__MODULE__{
        map: map,
        width: width,
        heigth: height
      }
    end
  end

  defmodule TraversableTreeMap do
    defstruct [:tree_map, :x_movement, :y_movement]

    def new(%TreeMap{} = tree_map, x_movement, y_movement) do
      %__MODULE__{
        tree_map: tree_map,
        x_movement: x_movement,
        y_movement: y_movement
      }
    end

    defimpl Enumerable do
      def count(_map), do: {:error, __MODULE__}
      def member?(_map, _elem), do: {:error, __MODULE__}
      def slice(_map), do: {:error, __MODULE__}

      def reduce(map, acc, fun) do
        walk(map, 0, 0, acc, fun)
      end

      defp walk(%Aoc.Day3.TraversableTreeMap{} = map, x, y, {:cont, acc}, fun) do
        if out_of_bounds?(map.tree_map.heigth, y) do
          {:done, acc}
        else
          actual_x = normalize_x(x, map.tree_map.width)
          element = get_in(map.tree_map.map, [Access.elem(y), Access.elem(actual_x)])

          new_acc = fun.(element, acc)

          walk(map, x + map.x_movement, y + map.y_movement, new_acc, fun)
        end
      end

      defp walk(_map, _x, _y, {:halt, acc}, _fun) do
        {:halted, acc}
      end

      defp walk(map, x, y, {:suspend, acc}, fun) do
        {:suspended, acc, &walk(map, x, y, &1, fun)}
      end

      defp out_of_bounds?(height, y) do
        y >= height
      end

      defp normalize_x(x, width) do
        Integer.mod(x, width)
      end
    end
  end

  def run_part_1 do
    @input
    |> File.read!()
    |> TreeMap.parse()
    |> TraversableTreeMap.new(3, 1)
    |> Enum.count(&(&1 == :tree))
  end

  def run_part_2 do
    tree_map =
      @input
      |> File.read!()
      |> TreeMap.parse()

    [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}]
    |> Enum.map(fn {x_movement, y_movement} ->
      tree_map
      |> TraversableTreeMap.new(x_movement, y_movement)
      |> Enum.count(&(&1 == :tree))
    end)
    |> Enum.reduce(1, &Kernel.*/2)
  end
end
