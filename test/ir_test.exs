defmodule BPF.IRTest do
  use ExUnit.Case, async: true

  alias BPF.IR.{Segment, Pattern, Guard, Clause}

  describe "Segment.literal/4" do
    test "creates literal segment with defaults" do
      seg = Segment.literal(42, 8, 0)
      assert seg.type == :literal
      assert seg.value == 42
      assert seg.size == 8
      assert seg.offset == 0
      assert seg.unit == 1
      assert seg.endianness == :big
      assert seg.signedness == :unsigned
    end

    test "creates literal segment with options" do
      seg = Segment.literal(0x1234, 16, 8, endianness: :little, unit: 8)
      assert seg.endianness == :little
      assert seg.unit == 8
    end
  end

  describe "Segment.binding/4" do
    test "creates binding segment with name" do
      seg = Segment.binding(:flags, 8, 4)
      assert seg.type == :binding
      assert seg.name == :flags
      assert seg.size == 8
      assert seg.offset == 4
    end

    test "creates binding with signedness" do
      seg = Segment.binding(:value, 16, 0, signedness: :signed)
      assert seg.signedness == :signed
    end
  end

  describe "Segment.skip/3" do
    test "creates skip segment" do
      seg = Segment.skip(8, 0)
      assert seg.type == :skip
      assert seg.name == nil
      assert seg.value == nil
      assert seg.size == 8
    end

    test "creates skip with rest size" do
      seg = Segment.skip(:rest, 16, unit: 8)
      assert seg.size == :rest
      assert seg.unit == 8
    end
  end

  describe "Segment.bits/1" do
    test "returns bit count for fixed size" do
      seg = Segment.binding(:x, 8, 0)
      assert Segment.bits(seg) == 8
    end

    test "applies unit multiplier" do
      seg = Segment.binding(:x, 2, 0, unit: 8)
      assert Segment.bits(seg) == 16
    end

    test "returns :rest for variable size" do
      seg = Segment.skip(:rest, 0)
      assert Segment.bits(seg) == :rest
    end
  end

  describe "Pattern.new/1" do
    test "creates pattern from segments" do
      segments = [
        Segment.literal(4, 4, 0),
        Segment.binding(:ihl, 4, 4)
      ]

      pattern = Pattern.new(segments)
      assert pattern.segments == segments
    end

    test "creates empty pattern" do
      pattern = Pattern.new([])
      assert pattern.segments == []
    end
  end

  describe "Pattern.bindings/1" do
    test "returns map of bindings" do
      segments = [
        Segment.literal(4, 4, 0),
        Segment.binding(:ihl, 4, 4),
        Segment.binding(:tos, 8, 8)
      ]

      pattern = Pattern.new(segments)
      bindings = Pattern.bindings(pattern)

      assert Map.has_key?(bindings, :ihl)
      assert Map.has_key?(bindings, :tos)
      assert bindings[:ihl].offset == 4
      assert bindings[:tos].offset == 8
    end

    test "returns empty map when no bindings" do
      segments = [
        Segment.literal(4, 4, 0),
        Segment.skip(4, 4)
      ]

      pattern = Pattern.new(segments)
      assert Pattern.bindings(pattern) == %{}
    end

    test "handles pattern with only bindings" do
      segments = [
        Segment.binding(:a, 8, 0),
        Segment.binding(:b, 8, 8),
        Segment.binding(:c, 8, 16)
      ]

      pattern = Pattern.new(segments)
      bindings = Pattern.bindings(pattern)

      assert map_size(bindings) == 3
    end
  end

  describe "Pattern.literals/1" do
    test "returns list of literals" do
      segments = [
        Segment.literal(4, 4, 0),
        Segment.binding(:ihl, 4, 4),
        Segment.literal(0, 8, 8)
      ]

      pattern = Pattern.new(segments)
      literals = Pattern.literals(pattern)

      assert length(literals) == 2
      assert Enum.any?(literals, &(&1.value == 4))
      assert Enum.any?(literals, &(&1.value == 0))
    end

    test "returns empty list when no literals" do
      segments = [Segment.binding(:x, 8, 0)]
      pattern = Pattern.new(segments)
      assert Pattern.literals(pattern) == []
    end
  end

  describe "Guard constructors" do
    test "compare/3 creates comparison" do
      guard = Guard.compare(:eq, {:binding, :x}, {:literal, 5})
      assert {:compare, :eq, {:binding, :x}, {:literal, 5}} = guard
    end

    test "logical_and/2 creates AND" do
      left = Guard.compare(:gt, {:binding, :x}, {:literal, 0})
      right = Guard.compare(:lt, {:binding, :x}, {:literal, 100})
      guard = Guard.logical_and(left, right)
      assert {:logical, :and, ^left, ^right} = guard
    end

    test "logical_or/2 creates OR" do
      left = Guard.compare(:eq, {:binding, :x}, {:literal, 1})
      right = Guard.compare(:eq, {:binding, :x}, {:literal, 2})
      guard = Guard.logical_or(left, right)
      assert {:logical, :or, ^left, ^right} = guard
    end

    test "logical_not/1 creates NOT" do
      inner = Guard.compare(:eq, {:binding, :x}, {:literal, 0})
      guard = Guard.logical_not(inner)
      assert {:logical, :not, ^inner, nil} = guard
    end

    test "bitwise/3 creates bitwise operation" do
      guard = Guard.bitwise(:band, {:binding, :x}, {:literal, 0xFF})
      assert {:bitwise, :band, {:binding, :x}, {:literal, 0xFF}} = guard
    end

    test "arith/3 creates arithmetic operation" do
      guard = Guard.arith(:add, {:binding, :x}, {:binding, :y})
      assert {:arith, :add, {:binding, :x}, {:binding, :y}} = guard
    end

    test "binding/1 creates binding reference" do
      assert {:binding, :foo} = Guard.binding(:foo)
    end

    test "literal/1 creates literal value" do
      assert {:literal, 42} = Guard.literal(42)
    end
  end

  describe "Clause.new/3" do
    test "creates clause with defaults" do
      pattern = Pattern.new([Segment.binding(:x, 8, 0)])
      clause = Clause.new(pattern)

      assert clause.pattern == pattern
      assert clause.guard == nil
      assert clause.action == :accept
    end

    test "creates clause with guard" do
      pattern = Pattern.new([Segment.binding(:x, 8, 0)])
      guard = Guard.compare(:eq, {:binding, :x}, {:literal, 5})
      clause = Clause.new(pattern, guard)

      assert clause.guard == guard
    end

    test "creates clause with reject action" do
      pattern = Pattern.new([])
      clause = Clause.new(pattern, nil, :reject)

      assert clause.action == :reject
    end

    test "creates clause with custom accept value" do
      pattern = Pattern.new([])
      clause = Clause.new(pattern, nil, {:accept, 65535})

      assert clause.action == {:accept, 65535}
    end
  end

  describe "complex guard expressions" do
    test "deeply nested logical operations" do
      # (a > 0 and b > 0) or (c > 0 and d > 0)
      a_check = Guard.compare(:gt, {:binding, :a}, {:literal, 0})
      b_check = Guard.compare(:gt, {:binding, :b}, {:literal, 0})
      c_check = Guard.compare(:gt, {:binding, :c}, {:literal, 0})
      d_check = Guard.compare(:gt, {:binding, :d}, {:literal, 0})

      left_and = Guard.logical_and(a_check, b_check)
      right_and = Guard.logical_and(c_check, d_check)
      guard = Guard.logical_or(left_and, right_and)

      assert {:logical, :or, {:logical, :and, _, _}, {:logical, :and, _, _}} = guard
    end

    test "arithmetic with bitwise operations" do
      # (x + y) band 0xFF
      sum = Guard.arith(:add, {:binding, :x}, {:binding, :y})
      guard = Guard.bitwise(:band, sum, {:literal, 0xFF})

      assert {:bitwise, :band, {:arith, :add, _, _}, {:literal, 0xFF}} = guard
    end
  end
end
