defmodule Advent.Day01 do
  @moduledoc """
  Day 1: Report Repair
  """

  @puzzle_input File.read!("#{__DIR__}/day01/puzzle_input.txt")

  @behaviour Advent.Day

  @impl true
  def part1 do
    {first, second} = @puzzle_input
                      |> decode()
                      |> find_where_double_sum(2020)

    first * second
  end

  @impl true
  def part2 do
    {first, second, third} = @puzzle_input
                      |> decode()
                      |> find_where_triple_sum(2020)

    first * second * third
  end

  @doc """
  Finds the combination of 2 values that give the specified sum.

  ## Example

      iex> find_where_double_sum([1721, 979, 366, 299, 675, 1456], 2020)
      {1721, 299}
  """
  @spec find_where_double_sum([integer], integer) :: {integer, integer}
  def find_where_double_sum(integers, sum) do
    integers = Enum.with_index(integers)

    combinations = for {v1, i1} <- integers,
                       {v2, i2} <- integers,
                       i2 > i1, do: {v1, v2}

    combinations
    |> Stream.filter(&(elem(&1, 0) + elem(&1, 1) == sum))
    |> Stream.take(1)
    |> Enum.into([])
    |> hd()
  end

  @doc """
  Finds the combination of 3 values that give the specified sum.

  ## Example

      iex> find_where_triple_sum([1721, 979, 366, 299, 675, 1456], 2020)
      {979, 366, 675}
  """
  @spec find_where_triple_sum([integer], integer) :: {integer, integer, integer}
  def find_where_triple_sum(integers, sum) do
    integers = Enum.with_index(integers)

    combinations = for {v1, i1} <- integers,
                       {v2, i2} <- integers,
                       {v3, i3} <- integers,
                       i2 > i1,
                       i3 != i1,
                       i3 != i2,
                       do: {v1, v2, v3}

    combinations
    |> Stream.filter(&(elem(&1, 0) + elem(&1, 1) + elem(&1, 2) == sum))
    |> Stream.take(1)
    |> Enum.into([])
    |> hd()
  end

  @doc """
  Decodes the given puzzle input into a list of integers.

  ## Example

      iex> decode("1721\\n979\\n366\\n299\\n675\\n1456\\n")
      [1721, 979, 366, 299, 675, 1456]
  """
  @spec decode(binary) :: [integer]
  def decode(raw) do
    raw
    |> String.split("\n")
    |> Stream.map(&String.trim/1)
    |> Stream.filter(&(&1 != ""))
    |> Stream.map(&String.to_integer/1)
    |> Enum.into([])
  end
end
