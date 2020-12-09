defmodule Advent.Day do
  @moduledoc """
  Any module implementing this behaviour ensures that define the same functions
  for a day's problem.
  """

  @doc """
  Returns the result of the first part of a day's problem.
  """
  @callback part1() :: term

  @doc """
  Returns the result of the second part of a day's problem.
  """
  @callback part2() :: term
end
