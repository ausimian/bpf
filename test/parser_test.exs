defmodule BPF.ParserTest do
  use ExUnit.Case, async: true

  alias BPF.Parser
  alias BPF.IR.{Pattern, Clause}

  describe "parse/1 basic patterns" do
    test "parses simple byte binding" do
      ast = quote do: fn <<x::8>> -> true end
      assert {:ok, [%Clause{} = clause]} = Parser.parse(ast)

      assert %Pattern{segments: [segment]} = clause.pattern
      assert segment.type == :binding
      assert segment.name == :x
      assert segment.size == 8
      assert segment.offset == 0
    end

    test "parses multiple bindings with offsets" do
      ast = quote do: fn <<a::8, b::16, c::8>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)

      assert [seg_a, seg_b, seg_c] = pattern.segments

      assert seg_a.name == :a
      assert seg_a.size == 8
      assert seg_a.offset == 0

      assert seg_b.name == :b
      assert seg_b.size == 16
      assert seg_b.offset == 8

      assert seg_c.name == :c
      assert seg_c.size == 8
      assert seg_c.offset == 24
    end

    test "parses literal values" do
      ast = quote do: fn <<4::4, 5::4>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)

      assert [seg1, seg2] = pattern.segments

      assert seg1.type == :literal
      assert seg1.value == 4
      assert seg1.size == 4
      assert seg1.offset == 0

      assert seg2.type == :literal
      assert seg2.value == 5
      assert seg2.size == 4
      assert seg2.offset == 4
    end

    test "parses skip segments" do
      ast = quote do: fn <<_::8, x::8, _::binary>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)

      assert [skip1, binding, skip2] = pattern.segments

      assert skip1.type == :skip
      assert skip1.size == 8
      assert skip1.offset == 0

      assert binding.type == :binding
      assert binding.name == :x
      assert binding.offset == 8

      assert skip2.type == :skip
      assert skip2.size == :rest
      assert skip2.offset == 16
    end

    test "parses endianness modifiers" do
      ast = quote do: fn <<x::16-little, y::16-big>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)

      assert [seg_x, seg_y] = pattern.segments

      assert seg_x.endianness == :little
      assert seg_y.endianness == :big
    end

    test "parses mixed literal and binding" do
      ast = quote do: fn <<4::4, ihl::4>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)

      assert [literal, binding] = pattern.segments

      assert literal.type == :literal
      assert literal.value == 4
      assert literal.size == 4

      assert binding.type == :binding
      assert binding.name == :ihl
      assert binding.size == 4
      assert binding.offset == 4
    end
  end

  describe "parse/1 guards" do
    test "parses equality guard" do
      ast = quote do: fn <<x::8>> when x == 5 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)

      assert {:compare, :eq, {:binding, :x}, {:literal, 5}} = guard
    end

    test "parses inequality guard" do
      ast = quote do: fn <<x::8>> when x != 0 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)

      assert {:compare, :neq, {:binding, :x}, {:literal, 0}} = guard
    end

    test "parses comparison guards" do
      ast = quote do: fn <<x::8>> when x > 10 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :gt, {:binding, :x}, {:literal, 10}} = guard

      ast = quote do: fn <<x::8>> when x >= 10 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :gte, {:binding, :x}, {:literal, 10}} = guard

      ast = quote do: fn <<x::8>> when x < 10 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :lt, {:binding, :x}, {:literal, 10}} = guard

      ast = quote do: fn <<x::8>> when x <= 10 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :lte, {:binding, :x}, {:literal, 10}} = guard
    end

    test "parses logical AND" do
      ast = quote do: fn <<x::8, y::8>> when x > 0 and y > 0 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)

      assert {:logical, :and, left, right} = guard
      assert {:compare, :gt, {:binding, :x}, {:literal, 0}} = left
      assert {:compare, :gt, {:binding, :y}, {:literal, 0}} = right
    end

    test "parses logical OR" do
      ast = quote do: fn <<x::8>> when x == 1 or x == 2 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)

      assert {:logical, :or, left, right} = guard
      assert {:compare, :eq, {:binding, :x}, {:literal, 1}} = left
      assert {:compare, :eq, {:binding, :x}, {:literal, 2}} = right
    end

    test "parses bitwise AND in guard operand" do
      ast = quote do: fn <<x::8>> when band(x, 0x0F) == 5 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)

      assert {:compare, :eq, {:bitwise, :band, {:binding, :x}, {:literal, 15}}, {:literal, 5}} =
               guard
    end

    test "parses arithmetic in guards" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 10 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)

      assert {:compare, :gt, {:arith, :add, {:binding, :x}, {:binding, :y}}, {:literal, 10}} =
               guard
    end
  end

  describe "parse/1 actions" do
    test "true returns accept" do
      ast = quote do: fn <<_::8>> -> true end
      assert {:ok, [%Clause{action: :accept}]} = Parser.parse(ast)
    end

    test "false returns reject" do
      ast = quote do: fn <<_::8>> -> false end
      assert {:ok, [%Clause{action: :reject}]} = Parser.parse(ast)
    end

    test "integer returns accept with value" do
      ast = quote do: fn <<_::8>> -> 65535 end
      assert {:ok, [%Clause{action: {:accept, 65535}}]} = Parser.parse(ast)
    end
  end

  describe "parse/1 multiple clauses" do
    test "parses multiple clauses" do
      # Note: clauses with just `_` won't parse as they're not binary patterns
      ast =
        quote do
          fn
            <<4::4, _::4, _::binary>> -> true
            <<6::4, _::4, _::binary>> -> true
          end
        end

      assert {:ok, [clause1, clause2]} = Parser.parse(ast)

      # First clause: IPv4 (version == 4)
      [literal1 | _] = clause1.pattern.segments
      assert literal1.value == 4

      # Second clause: IPv6 (version == 6)
      [literal2 | _] = clause2.pattern.segments
      assert literal2.value == 6
    end
  end

  describe "Pattern helper functions" do
    test "bindings/1 returns all bindings" do
      ast = quote do: fn <<4::4, ihl::4, tos::8, len::16>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)

      bindings = Pattern.bindings(pattern)

      assert Map.has_key?(bindings, :ihl)
      assert Map.has_key?(bindings, :tos)
      assert Map.has_key?(bindings, :len)
      refute Map.has_key?(bindings, :version)

      assert bindings[:ihl].offset == 4
      assert bindings[:tos].offset == 8
      assert bindings[:len].offset == 16
    end

    test "literals/1 returns all literals" do
      ast = quote do: fn <<4::4, _::4, 0::8>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)

      literals = Pattern.literals(pattern)
      assert length(literals) == 2
      assert Enum.any?(literals, &(&1.value == 4))
      assert Enum.any?(literals, &(&1.value == 0))
    end
  end

  describe "parse/1 error cases" do
    test "returns error for non-function input" do
      assert {:error, {:expected_fn, _}} = Parser.parse(:not_a_function)
    end

    test "returns error for non-binary pattern" do
      ast = quote do: fn x -> true end
      assert {:error, {:expected_binary_pattern, _}} = Parser.parse(ast)
    end

    test "returns error for invalid action" do
      # Manually construct AST with invalid body
      # fn clauses are in the format: [{:->, _, [[pattern], body]}]
      ast = {:fn, [], [{:->, [], [[{:<<>>, [], [{:"::", [], [{:_, [], nil}, 8]}]}], :invalid_action]}]}
      assert {:error, {:invalid_action, :invalid_action}} = Parser.parse(ast)
    end

    test "returns error for invalid guard expression" do
      # A guard with an unsupported operator
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:invalid_op, [], [{:x, [], nil}, 5]}]}], true]}]}
      assert {:error, {:invalid_guard, _}} = Parser.parse(ast)
    end

    test "returns error for invalid operand in guard" do
      # A guard using a function call as operand
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:==, [], [{:foo, [], [{:x, [], nil}]}, 5]}]}], true]}]}
      assert {:error, {:invalid_operand, _}} = Parser.parse(ast)
    end

    test "returns error for variable size specification" do
      # <<x::n>> where n is a variable - not supported in BPF
      ast = {:fn, [], [{:->, [], [[{:<<>>, [], [{:"::", [], [{:x, [], nil}, {:n, [], nil}]}]}], true]}]}
      assert {:error, {:variable_size_not_supported, :n}} = Parser.parse(ast)
    end
  end

  describe "parse/1 bnot operator" do
    test "parses bnot in guard" do
      ast = quote do: fn <<x::8>> when bnot(x) == -1 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :eq, {:bitwise, :bnot, {:binding, :x}, nil}, {:literal, -1}} = guard
    end

    test "parses negative literal in guard" do
      ast = quote do: fn <<x::8>> when x > -10 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :gt, {:binding, :x}, {:literal, -10}} = guard
    end
  end

  describe "parse/1 empty pattern" do
    test "parses empty binary pattern" do
      ast = quote do: fn <<>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      assert pattern.segments == []
    end
  end

  describe "parse/1 complex modifiers" do
    test "parses unit specification" do
      ast = quote do: fn <<x::size(2)-unit(8)>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.size == 2
      assert seg.unit == 8
    end
  end

  describe "parse/1 size specifications" do
    test "parses native endianness" do
      ast = quote do: fn <<x::16-native>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.endianness == :native
    end

    test "parses signed modifier" do
      ast = quote do: fn <<x::8-signed>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.signedness == :signed
    end

    test "parses unsigned modifier" do
      ast = quote do: fn <<x::8-unsigned>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.signedness == :unsigned
    end

    test "parses bits type" do
      ast = quote do: fn <<_::bits>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.size == :rest
      assert seg.unit == 1
    end

    test "parses bytes type" do
      ast = quote do: fn <<_::bytes>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.size == :rest
      assert seg.unit == 8
    end

    test "parses binary-size(n) combination" do
      ast = quote do: fn <<_::binary-size(10), x::8>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [skip, binding] = pattern.segments
      assert skip.size == 10
      assert skip.unit == 8
      assert binding.name == :x
    end

    test "parses size(n) specification" do
      ast = quote do: fn <<x::size(32)>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.size == 32
    end

    test "parses combined modifiers" do
      ast = quote do: fn <<x::16-little-signed>> -> true end
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.endianness == :little
      assert seg.signedness == :signed
    end
  end

  describe "parse/1 guard edge cases" do
    test "parses subtraction in guard" do
      ast = quote do: fn <<x::8, y::8>> when x - y > 0 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :gt, {:arith, :sub, {:binding, :x}, {:binding, :y}}, {:literal, 0}} = guard
    end

    test "parses multiplication in guard" do
      ast = quote do: fn <<x::8>> when x * 2 > 100 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :gt, {:arith, :mul, {:binding, :x}, {:literal, 2}}, {:literal, 100}} = guard
    end

    test "parses division in guard" do
      ast = quote do: fn <<x::8>> when div(x, 2) > 10 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :gt, {:arith, :div, {:binding, :x}, {:literal, 2}}, {:literal, 10}} = guard
    end

    test "parses bor in guard" do
      ast = quote do: fn <<x::8>> when bor(x, 0xF0) == 0xFF -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :eq, {:bitwise, :bor, {:binding, :x}, {:literal, 0xF0}}, {:literal, 0xFF}} =
               guard
    end

    test "parses bxor in guard" do
      ast = quote do: fn <<x::8>> when bxor(x, 0xFF) == 0 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :eq, {:bitwise, :bxor, {:binding, :x}, {:literal, 0xFF}}, {:literal, 0}} =
               guard
    end

    test "parses deeply nested logical expressions" do
      ast = quote do: fn <<a::8, b::8, c::8>> when (a > 0 and b > 0) or c > 0 -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:logical, :or, {:logical, :and, _, _}, _} = guard
    end

    test "parses comparison with binding on right side" do
      ast = quote do: fn <<x::8, y::8>> when x > y -> true end
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :gt, {:binding, :x}, {:binding, :y}} = guard
    end
  end

  describe "parse/1 action edge cases" do
    test "zero returns reject" do
      ast = quote do: fn <<_::8>> -> 0 end
      assert {:ok, [%Clause{action: :reject}]} = Parser.parse(ast)
    end

    test ":accept atom returns accept" do
      ast = quote do: fn <<_::8>> -> :accept end
      assert {:ok, [%Clause{action: :accept}]} = Parser.parse(ast)
    end

    test ":reject atom returns reject" do
      ast = quote do: fn <<_::8>> -> :reject end
      assert {:ok, [%Clause{action: :reject}]} = Parser.parse(ast)
    end

    test "large integer returns accept with value" do
      ast = quote do: fn <<_::8>> -> 0xFFFFFFFF end
      assert {:ok, [%Clause{action: {:accept, 0xFFFFFFFF}}]} = Parser.parse(ast)
    end
  end

  describe "parse/1 bare expression (no size spec)" do
    test "parses bare binding defaults to 8 bits" do
      # When no size spec is given, defaults to 8 bits
      ast = {:fn, [], [{:->, [], [[{:<<>>, [], [{:x, [], nil}]}], true]}]}
      assert {:ok, [%Clause{pattern: pattern}]} = Parser.parse(ast)
      [seg] = pattern.segments
      assert seg.type == :binding
      assert seg.name == :x
      assert seg.size == 8
    end
  end

  describe "parse/1 error edge cases" do
    test "returns error for invalid segment expression" do
      # Using a tuple inside the segment which is not a valid expression
      ast = {:fn, [], [{:->, [], [[{:<<>>, [], [{:"::", [], [{{:tuple}, []}, 8]}]}], true]}]}
      assert {:error, {:invalid_segment_expr, _}} = Parser.parse(ast)
    end

    test "returns error for invalid size spec" do
      # Using a list as size spec which is invalid
      ast = {:fn, [], [{:->, [], [[{:<<>>, [], [{:"::", [], [{:x, [], nil}, [1, 2, 3]]}]}], true]}]}
      assert {:error, {:invalid_size_spec, [1, 2, 3]}} = Parser.parse(ast)
    end

    test "returns error for invalid modifier" do
      # Using an unknown modifier like 'foo'
      ast = {:fn, [], [{:->, [], [[{:<<>>, [], [{:"::", [], [{:x, [], nil}, {:-, [], [8, {:foo, [], nil}]}]}]}], true]}]}
      assert {:error, {:invalid_modifier, {:foo, [], nil}}} = Parser.parse(ast)
    end
  end

  describe "parse/1 Bitwise module qualified calls" do
    test "parses Bitwise.band in guard" do
      # Bitwise.band(x, 0x0F)
      band_call = {{:., [], [{:__aliases__, [], [:Bitwise]}, :band]}, [], [{:x, [], nil}, 0x0F]}
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:==, [], [band_call, 0x0F]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :eq, {:bitwise, :band, {:binding, :x}, {:literal, 15}}, {:literal, 15}} = guard
    end

    test "parses Bitwise.bor in guard" do
      # Bitwise.bor(x, 0x80)
      bor_call = {{:., [], [{:__aliases__, [], [:Bitwise]}, :bor]}, [], [{:x, [], nil}, 0x80]}
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:==, [], [bor_call, 0x80]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :eq, {:bitwise, :bor, {:binding, :x}, {:literal, 128}}, {:literal, 128}} = guard
    end

    test "parses Bitwise.bxor in guard" do
      # Bitwise.bxor(x, 0xFF)
      bxor_call = {{:., [], [{:__aliases__, [], [:Bitwise]}, :bxor]}, [], [{:x, [], nil}, 0xFF]}
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:==, [], [bxor_call, 0]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :eq, {:bitwise, :bxor, {:binding, :x}, {:literal, 255}}, {:literal, 0}} = guard
    end

    test "parses Bitwise.bnot in guard" do
      # Bitwise.bnot(x)
      bnot_call = {{:., [], [{:__aliases__, [], [:Bitwise]}, :bnot]}, [], [{:x, [], nil}]}
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:==, [], [bnot_call, 0]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:compare, :eq, {:bitwise, :bnot, {:binding, :x}, nil}, {:literal, 0}} = guard
    end
  end

  describe "parse/1 imported bitwise as guards (top-level)" do
    test "parses band as top-level guard (checking truthiness)" do
      # when band(x, 0x80) - this is band as a guard expression, not inside comparison
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:band, [], [{:x, [], nil}, 0x80]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:bitwise, :band, {:binding, :x}, {:literal, 128}} = guard
    end

    test "parses bor as top-level guard" do
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:bor, [], [{:x, [], nil}, 0x80]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:bitwise, :bor, {:binding, :x}, {:literal, 128}} = guard
    end

    test "parses bxor as top-level guard" do
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:bxor, [], [{:x, [], nil}, 0xFF]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:bitwise, :bxor, {:binding, :x}, {:literal, 255}} = guard
    end

    test "parses bnot as top-level guard" do
      ast = {:fn, [], [{:->, [], [[{:when, [], [{:<<>>, [], [{:"::", [], [{:x, [], nil}, 8]}]}, {:bnot, [], [{:x, [], nil}]}]}], true]}]}
      assert {:ok, [%Clause{guard: guard}]} = Parser.parse(ast)
      assert {:bitwise, :bnot, {:binding, :x}, nil} = guard
    end
  end
end
