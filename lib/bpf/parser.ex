defmodule BPF.Parser do
  @moduledoc """
  Parses Elixir AST from binary pattern matching functions into IR.

  Takes quoted expressions like:
      quote do: fn <<4::4, flags::4, _::binary>> when flags == 8 -> true end

  And produces `BPF.IR.Clause` structs.
  """

  alias BPF.IR.{Segment, Pattern, Guard, Clause}

  @doc """
  Parse a quoted function expression into a list of clauses.

  ## Example

      iex> ast = quote do: fn <<x::8>> -> true end
      iex> BPF.Parser.parse(ast)
      {:ok, [%BPF.IR.Clause{...}]}
  """
  def parse({:fn, _, clauses}) do
    parsed =
      clauses
      |> Enum.map(&parse_clause/1)
      |> collect_results()

    case parsed do
      {:ok, clauses} -> {:ok, clauses}
      {:error, _} = error -> error
    end
  end

  def parse(other) do
    {:error, {:expected_fn, other}}
  end

  defp collect_results(results) do
    Enum.reduce_while(results, {:ok, []}, fn
      {:ok, clause}, {:ok, acc} -> {:cont, {:ok, acc ++ [clause]}}
      {:error, _} = error, _ -> {:halt, error}
    end)
  end

  # Parse a single clause: {:->, meta, [[pattern_or_when], body]}
  defp parse_clause({:->, _, [[{:when, _, [pattern, guard]}], body]}) do
    with {:ok, parsed_pattern} <- parse_pattern(pattern),
         {:ok, parsed_guard} <- parse_guard(guard),
         {:ok, action} <- parse_action(body) do
      {:ok, Clause.new(parsed_pattern, parsed_guard, action)}
    end
  end

  defp parse_clause({:->, _, [[pattern], body]}) do
    with {:ok, parsed_pattern} <- parse_pattern(pattern),
         {:ok, action} <- parse_action(body) do
      {:ok, Clause.new(parsed_pattern, nil, action)}
    end
  end

  # Parse a binary pattern: {:<<>>, meta, segments}
  defp parse_pattern({:<<>>, _, segments}) do
    parse_segments(segments, 0, [])
  end

  defp parse_pattern(other) do
    {:error, {:expected_binary_pattern, other}}
  end

  # Parse segments recursively, tracking bit offset
  defp parse_segments([], _offset, acc) do
    {:ok, Pattern.new(Enum.reverse(acc))}
  end

  defp parse_segments([segment | rest], offset, acc) do
    case parse_segment(segment, offset) do
      {:ok, parsed, next_offset} ->
        parse_segments(rest, next_offset, [parsed | acc])

      {:error, _} = error ->
        error
    end
  end

  # Parse a segment: {:"::", meta, [expr, size_spec]}
  defp parse_segment({:"::", _, [expr, size_spec]}, offset) do
    with {:ok, {type, name, value}} <- parse_segment_expr(expr),
         {:ok, size, opts} <- parse_size_spec(size_spec) do
      segment =
        case type do
          :literal -> Segment.literal(value, size, offset, opts)
          :binding -> Segment.binding(name, size, offset, opts)
          :skip -> Segment.skip(size, offset, opts)
        end

      next_offset =
        case size do
          :rest -> offset
          n -> offset + n * Keyword.get(opts, :unit, 1)
        end

      {:ok, segment, next_offset}
    end
  end

  # Bare expression without size spec (defaults to 8 bits for integers)
  defp parse_segment(expr, offset) do
    parse_segment({:"::", [], [expr, 8]}, offset)
  end

  # Parse the expression part of a segment
  defp parse_segment_expr(value) when is_integer(value) do
    {:ok, {:literal, nil, value}}
  end

  defp parse_segment_expr({:_, _, _}) do
    {:ok, {:skip, nil, nil}}
  end

  defp parse_segment_expr({name, _, context}) when is_atom(name) and is_atom(context) do
    {:ok, {:binding, name, nil}}
  end

  defp parse_segment_expr(other) do
    {:error, {:invalid_segment_expr, other}}
  end

  # Parse size specification
  # Simple integer size: 8
  defp parse_size_spec(size) when is_integer(size) do
    {:ok, size, []}
  end

  # Size with modifiers: 16-big, 8-little-signed, binary-size(12)
  defp parse_size_spec({:-, _, [left, right]}) do
    with {:ok, size, left_opts} <- parse_size_spec(left),
         {:ok, right_size, right_opts} <- parse_size_or_modifier(right) do
      # If right side specifies a size, use it (for binary-size(n))
      final_size = if right_size, do: right_size, else: size
      {:ok, final_size, Keyword.merge(left_opts, right_opts)}
    end
  end

  # Named types: binary, bits, etc.
  defp parse_size_spec({:binary, _, _}) do
    {:ok, :rest, [unit: 8]}
  end

  defp parse_size_spec({:bits, _, _}) do
    {:ok, :rest, [unit: 1]}
  end

  defp parse_size_spec({:bytes, _, _}) do
    {:ok, :rest, [unit: 8]}
  end

  # Size expression: size(n)
  defp parse_size_spec({:size, _, [n]}) when is_integer(n) do
    {:ok, n, []}
  end

  # Unit expression: unit(n)
  defp parse_size_spec({:unit, _, [n]}) when is_integer(n) do
    {:ok, 1, [unit: n]}
  end

  # Variable size (not supported in BPF)
  defp parse_size_spec({name, _, _}) when is_atom(name) do
    {:error, {:variable_size_not_supported, name}}
  end

  defp parse_size_spec(other) do
    {:error, {:invalid_size_spec, other}}
  end

  # Handle both size() and modifiers for combined expressions like binary-size(12)
  defp parse_size_or_modifier({:size, _, [n]}) when is_integer(n), do: {:ok, n, []}
  defp parse_size_or_modifier({:unit, _, [n]}) when is_integer(n), do: {:ok, nil, [unit: n]}

  defp parse_size_or_modifier(other) do
    case parse_modifier(other) do
      {:ok, opts} -> {:ok, nil, opts}
      error -> error
    end
  end

  # Parse modifiers like :big, :little, :signed, :unsigned
  defp parse_modifier({:big, _, _}), do: {:ok, [endianness: :big]}
  defp parse_modifier({:little, _, _}), do: {:ok, [endianness: :little]}
  defp parse_modifier({:native, _, _}), do: {:ok, [endianness: :native]}
  defp parse_modifier({:signed, _, _}), do: {:ok, [signedness: :signed]}
  defp parse_modifier({:unsigned, _, _}), do: {:ok, [signedness: :unsigned]}
  defp parse_modifier(other), do: {:error, {:invalid_modifier, other}}

  # Parse guard expressions
  defp parse_guard(ast) do
    do_parse_guard(ast)
  end

  # Comparison operators
  defp do_parse_guard({:==, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.compare(:eq, l, r)}
    end
  end

  defp do_parse_guard({:!=, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.compare(:neq, l, r)}
    end
  end

  defp do_parse_guard({:<, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.compare(:lt, l, r)}
    end
  end

  defp do_parse_guard({:>, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.compare(:gt, l, r)}
    end
  end

  defp do_parse_guard({:<=, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.compare(:lte, l, r)}
    end
  end

  defp do_parse_guard({:>=, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.compare(:gte, l, r)}
    end
  end

  # Logical operators
  defp do_parse_guard({:and, _, [left, right]}) do
    with {:ok, l} <- do_parse_guard(left),
         {:ok, r} <- do_parse_guard(right) do
      {:ok, Guard.logical_and(l, r)}
    end
  end

  defp do_parse_guard({:or, _, [left, right]}) do
    with {:ok, l} <- do_parse_guard(left),
         {:ok, r} <- do_parse_guard(right) do
      {:ok, Guard.logical_or(l, r)}
    end
  end

  defp do_parse_guard({:not, _, [expr]}) do
    with {:ok, e} <- do_parse_guard(expr) do
      {:ok, Guard.logical_not(e)}
    end
  end

  # Bitwise operators (Elixir uses Bitwise module functions in guards)
  defp do_parse_guard({{:., _, [{:__aliases__, _, [:Bitwise]}, :band]}, _, [left, right]}) do
    parse_bitwise(:band, left, right)
  end

  defp do_parse_guard({{:., _, [{:__aliases__, _, [:Bitwise]}, :bor]}, _, [left, right]}) do
    parse_bitwise(:bor, left, right)
  end

  defp do_parse_guard({{:., _, [{:__aliases__, _, [:Bitwise]}, :bxor]}, _, [left, right]}) do
    parse_bitwise(:bxor, left, right)
  end

  defp do_parse_guard({{:., _, [{:__aliases__, _, [:Bitwise]}, :bnot]}, _, [expr]}) do
    with {:ok, e} <- parse_operand(expr) do
      {:ok, Guard.bitwise(:bnot, e, nil)}
    end
  end

  # Also handle imported Bitwise functions (band/2, bor/2, etc.)
  defp do_parse_guard({:band, _, [left, right]}), do: parse_bitwise(:band, left, right)
  defp do_parse_guard({:bor, _, [left, right]}), do: parse_bitwise(:bor, left, right)
  defp do_parse_guard({:bxor, _, [left, right]}), do: parse_bitwise(:bxor, left, right)
  defp do_parse_guard({:bnot, _, [expr]}) do
    with {:ok, e} <- parse_operand(expr) do
      {:ok, Guard.bitwise(:bnot, e, nil)}
    end
  end

  # Arithmetic operators when used in guards (nested in comparisons)
  defp do_parse_guard(other) do
    {:error, {:invalid_guard, other}}
  end

  defp parse_bitwise(op, left, right) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.bitwise(op, l, r)}
    end
  end

  # Parse an operand (can be a binding, literal, or expression)
  defp parse_operand(value) when is_integer(value) do
    {:ok, Guard.literal(value)}
  end

  # Handle unary minus for negative literals
  defp parse_operand({:-, _, [value]}) when is_integer(value) do
    {:ok, Guard.literal(-value)}
  end

  defp parse_operand({name, _, context}) when is_atom(name) and is_atom(context) do
    {:ok, Guard.binding(name)}
  end

  # Arithmetic expressions as operands
  defp parse_operand({:+, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.arith(:add, l, r)}
    end
  end

  defp parse_operand({:-, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.arith(:sub, l, r)}
    end
  end

  defp parse_operand({:*, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.arith(:mul, l, r)}
    end
  end

  defp parse_operand({:div, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.arith(:div, l, r)}
    end
  end

  # Bitwise operations as operands (for expressions like `band(x, 0x0F) == 5`)
  defp parse_operand({:band, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.bitwise(:band, l, r)}
    end
  end

  defp parse_operand({:bor, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.bitwise(:bor, l, r)}
    end
  end

  defp parse_operand({:bxor, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.bitwise(:bxor, l, r)}
    end
  end

  defp parse_operand({:bnot, _, [expr]}) do
    with {:ok, e} <- parse_operand(expr) do
      {:ok, Guard.bitwise(:bnot, e, nil)}
    end
  end

  # Bitwise module qualified calls as operands (e.g., Bitwise.band(x, 0x0F))
  defp parse_operand({{:., _, [{:__aliases__, _, [:Bitwise]}, :band]}, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.bitwise(:band, l, r)}
    end
  end

  defp parse_operand({{:., _, [{:__aliases__, _, [:Bitwise]}, :bor]}, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.bitwise(:bor, l, r)}
    end
  end

  defp parse_operand({{:., _, [{:__aliases__, _, [:Bitwise]}, :bxor]}, _, [left, right]}) do
    with {:ok, l} <- parse_operand(left),
         {:ok, r} <- parse_operand(right) do
      {:ok, Guard.bitwise(:bxor, l, r)}
    end
  end

  defp parse_operand({{:., _, [{:__aliases__, _, [:Bitwise]}, :bnot]}, _, [expr]}) do
    with {:ok, e} <- parse_operand(expr) do
      {:ok, Guard.bitwise(:bnot, e, nil)}
    end
  end

  defp parse_operand(other) do
    {:error, {:invalid_operand, other}}
  end

  # Parse return action from function body
  defp parse_action(true), do: {:ok, :accept}
  defp parse_action(false), do: {:ok, :reject}
  defp parse_action(:accept), do: {:ok, :accept}
  defp parse_action(:reject), do: {:ok, :reject}
  defp parse_action(n) when is_integer(n) and n > 0, do: {:ok, {:accept, n}}
  defp parse_action(0), do: {:ok, :reject}
  defp parse_action(other), do: {:error, {:invalid_action, other}}
end
