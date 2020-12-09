defmodule Advent.Day02 do
  @moduledoc """
  Day 2: Password Philosophy
  """

  @puzzle_input File.read!("#{__DIR__}/day02/puzzle_input.txt")

  @typedoc"""
  The type of a password policy.
  """
  @type policy :: {Range.t, char}

  @typedoc """
  A password in a textual representation.
  """
  @type password :: String.t

  @behaviour Advent.Day

  @impl true
  def part1 do
    @puzzle_input
    |> decode()
    |> Enum.count(fn {policy, password} ->
      valid_sled_shop_password?(policy, password)
    end)
  end

  @impl true
  def part2 do
    @puzzle_input
    |> decode()
    |> Enum.count(fn {policy, password} ->
      valid_toboggan_shop_password?(policy, password)
    end)
  end

  @doc """
  Indicates if the given password is valid according to the provided sled rental
  password policy.

  ## Examples

      iex> valid_sled_shop_password?({1..3, "a"}, "abcde")
      true

      iex> valid_sled_shop_password?({1..3, "b"}, "cdefg")
      false

      iex> valid_sled_shop_password?({2..9, "c"}, "ccccccccc")
      true
  """
  @spec valid_sled_shop_password?(policy, password) :: boolean
  def valid_sled_shop_password?({range, letter}, password) do
    frequencies = password |> String.split("") |> Enum.frequencies()
    count = Map.get(frequencies, letter, 0)
    Enum.member?(range, count)
  end

  @doc """
  Indicates if the given password is valid according to the provided Official
  Toboggan Corporate Policy.

  ## Examples

      iex> valid_toboggan_shop_password?({1..3, "a"}, "abcde")
      true

      iex> valid_toboggan_shop_password?({1..3, "b"}, "cdefg")
      false

      iex> valid_toboggan_shop_password?({2..9, "c"}, "ccccccccc")
      false
  """
  @spec valid_toboggan_shop_password?(policy, password) :: boolean
  def valid_toboggan_shop_password?({range, letter}, password) do
    first_position = String.at(password, range.first - 1)
    second_position = String.at(password, range.last - 1)

    at_first? = first_position == letter
    at_second? = second_position == letter

    case {at_first?, at_second?} do
      {true, false} -> true
      {false, true} -> true
      _ -> false
    end
  end

  @doc """
  Decodes the given string into a list of policies and passwords.

  ## Example

      iex> decode("1-3 a: abcde\\n1-3 b: cdefg\\n2-9 c: ccccccccc")
      [
        {{1..3, "a"}, "abcde"},
        {{1..3, "b"}, "cdefg"},
        {{2..9, "c"}, "ccccccccc"}
      ]
  """
  @spec decode(binary) :: [{policy, password}]
  def decode(binary) do
    binary
    |> String.split("\n")
    |> Stream.map(&String.trim/1)
    |> Stream.filter(&(&1 != ""))
    |> Stream.map(&decode_line/1)
    |> Enum.into([])
  end

  defp decode_line(line) do
    case String.split(line, ":", parts: 3) do
      [policy, password] -> {decode_policy(String.trim(policy)), String.trim(password)}
      _ -> raise ArgumentError, message: "Invalid line: #{line}"
    end
  end

  @spec decode_policy(binary) :: policy
  defp decode_policy(line) do
    case String.split(line, " ", parts: 3) do
      [range, letter] -> {decode_range(range), letter}
      _ -> raise ArgumentError, message: "Invalid policy: #{line}"
    end
  end

  @spec decode_range(binary) :: Range.t
  defp decode_range(line) do
    case String.split(line, "-", parts: 3) do
      [min, max] -> Range.new(String.to_integer(min), String.to_integer(max))
      _ -> raise ArgumentError, message: "Invalid range: #{line}"
    end
  end
end
