defmodule Advent.Day03.Slope do
  @moduledoc """
  A mountain slop full of trees.
  """

  defstruct [trees: MapSet.new(), width: 0, height: 0]

  @type position :: {row :: integer, col :: integer}

  @type t :: %__MODULE__{
               trees: MapSet.t(position),
               width: integer,
               height: integer,
             }

  @doc """
  Creates a new slope
  """
  @spec new :: t
  def new do
    %__MODULE__{}
  end

  def add_tree(slope, position) do
    %{slope| trees: MapSet.put(slope.trees, position)}
  end

  def decode(binary) do
    lines = decode_lines(binary)

    slope = new()
    slope = %{slope|height: length(lines)}

    Enum.reduce(lines, slope, &add_tree/2)
  end

  defp decode_lines(binary) do
    binary
    |> String.trim()
    |> String.split("\n")
    |> Stream.map(&String.trim/1)
    |> String.filter(&(&1 != ""))
    |> Stream.with_index()
    |> Stream.flat_map(fn {row, row_index} ->

    end)
  end

  defp decode_row({row, row_index}) do

  end
end
