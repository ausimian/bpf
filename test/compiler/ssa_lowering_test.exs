defmodule BPF.Compiler.SSALoweringTest do
  use ExUnit.Case, async: true

  alias BPF.Compiler.SSA

  describe "from_clause/2 - pattern literal checks" do
    test "compiles byte-aligned literal" do
      ast = quote do: fn <<42::8>> -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :byte, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 42}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "compiles 16-bit literal" do
      ast = quote do: fn <<0x1234::16>> -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :half, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 0x1234}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "compiles 32-bit literal" do
      ast = quote do: fn <<0xDEADBEEF::32>> -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :word, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 0xDEADBEEF}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "compiles sub-byte literal with shift and mask" do
      ast = quote do: fn <<4::4, _::4>> -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # 4::4 at bit offset 0 needs shift 4, mask 0xF
      assert [
               {:load_packet, 0, :byte, 0, 4, 15},
               {:cmp, :eq, 0, {:imm, 4}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end
  end

  describe "from_clause/2 - pattern bindings" do
    test "compiles byte binding" do
      ast = quote do: fn <<x::8>> when x == 1 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :byte, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 1}, :fail},
               {:ret, :accept}
             ] = ssa.ops

      assert ssa.bindings == %{:x => 0}
    end

    test "compiles 16-bit binding" do
      ast = quote do: fn <<port::16>> when port > 1024 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :half, 0, 0, nil},
               {:cmp, :gt, 0, {:imm, 1024}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "compiles 32-bit binding" do
      ast = quote do: fn <<addr::32>> when addr == 0x7F000001 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :word, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 0x7F000001}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "compiles sub-byte binding" do
      ast = quote do: fn <<_::4, flags::4>> when flags == 8 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # flags at bit offset 4, shift 0, mask 0xF
      assert [
               {:load_packet, 0, :byte, 0, 0, 15},
               {:cmp, :eq, 0, {:imm, 8}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "skips unused bindings" do
      ast = quote do: fn <<x::8, _y::8, _z::8>> when x == 1 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Only x should be loaded
      load_ops =
        Enum.filter(ssa.ops, fn
          {:load_packet, _, _, _, _, _} -> true
          _ -> false
        end)

      assert length(load_ops) == 1
    end
  end

  describe "from_clause/2 - guard comparisons" do
    test "compiles neq comparison" do
      ast = quote do: fn <<x::8>> when x != 0 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:cmp, :neq, 0, {:imm, 0}, :fail} = Enum.at(ssa.ops, 1)
    end

    test "compiles gte comparison" do
      ast = quote do: fn <<x::8>> when x >= 10 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:cmp, :gte, 0, {:imm, 10}, :fail} = Enum.at(ssa.ops, 1)
    end

    test "compiles lte comparison" do
      ast = quote do: fn <<x::8>> when x <= 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:cmp, :lte, 0, {:imm, 100}, :fail} = Enum.at(ssa.ops, 1)
    end

    test "compiles lt comparison" do
      ast = quote do: fn <<x::8>> when x < 50 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:cmp, :lt, 0, {:imm, 50}, :fail} = Enum.at(ssa.ops, 1)
    end

    test "compiles comparison with two bindings" do
      ast = quote do: fn <<x::8, y::8>> when x > y -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Should compare vreg to vreg
      assert {:cmp, :gt, 0, 1, :fail} = Enum.at(ssa.ops, 2)
    end

    test "compiles comparison with literal on left side" do
      ast = quote do: fn <<x::8>> when 10 < x -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Literal loads to vreg, then comparison
      assert {:load_imm, _, 10} = Enum.at(ssa.ops, 1)
    end
  end

  describe "from_clause/2 - logical operators" do
    test "compiles AND with short-circuit" do
      ast = quote do: fn <<x::8>> when x > 5 and x < 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Both comparisons should jump to fail
      cmp_ops =
        Enum.filter(ssa.ops, fn
          {:cmp, _, _, _, _} -> true
          _ -> false
        end)

      assert length(cmp_ops) == 2
      assert Enum.all?(cmp_ops, fn {:cmp, _, _, _, label} -> label == :fail end)
    end

    test "compiles OR with short-circuit" do
      ast = quote do: fn <<x::8>> when x == 0 or x == 255 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # First comparison uses cmp_success to jump to success label
      assert {:cmp_success, :eq, _, _, _} = Enum.at(ssa.ops, 1)
      # Second uses regular cmp to fail label
      assert {:cmp, :eq, _, _, :fail} = Enum.at(ssa.ops, 2)
    end

    test "compiles nested OR" do
      ast = quote do: fn <<x::8>> when x == 1 or x == 2 or x == 3 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      cmp_success_ops =
        Enum.filter(ssa.ops, fn
          {:cmp_success, _, _, _, _} -> true
          _ -> false
        end)

      # First two branches use cmp_success
      assert length(cmp_success_ops) == 2
    end

    test "compiles AND within OR" do
      ast = quote do: fn <<x::8, y::8>> when (x > 0 and y > 0) or x == 255 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Should have a jump to success label after AND succeeds
      jump_ops =
        Enum.filter(ssa.ops, fn
          {:jump, _} -> true
          _ -> false
        end)

      assert length(jump_ops) >= 1
    end
  end

  describe "from_clause/2 - arithmetic" do
    test "compiles addition" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 2, :add, 0, 1} = Enum.at(ssa.ops, 2)
    end

    test "compiles subtraction" do
      ast = quote do: fn <<x::8, y::8>> when x - y > 0 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 2, :sub, 0, 1} = Enum.at(ssa.ops, 2)
    end

    test "compiles multiplication" do
      ast = quote do: fn <<x::8, y::8>> when x * y < 255 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 2, :mul, 0, 1} = Enum.at(ssa.ops, 2)
    end

    test "compiles division" do
      ast = quote do: fn <<x::8, y::8>> when div(x, y) > 0 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 2, :div, 0, 1} = Enum.at(ssa.ops, 2)
    end

    test "compiles arithmetic with immediate" do
      ast = quote do: fn <<x::8>> when x + 10 > 50 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 1, :add, 0, {:imm, 10}} = Enum.at(ssa.ops, 1)
    end
  end

  describe "from_clause/2 - bitwise operations" do
    test "compiles band" do
      ast = quote do: fn <<x::8>> when Bitwise.band(x, 0x0F) == 5 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 1, :band, 0, {:imm, 15}} = Enum.at(ssa.ops, 1)
    end

    test "compiles bor" do
      ast = quote do: fn <<x::8>> when Bitwise.bor(x, 0x80) > 127 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 1, :bor, 0, {:imm, 128}} = Enum.at(ssa.ops, 1)
    end

    test "compiles bxor" do
      ast = quote do: fn <<x::8>> when Bitwise.bxor(x, 0xFF) == 0 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 1, :bxor, 0, {:imm, 255}} = Enum.at(ssa.ops, 1)
    end

    test "compiles bnot" do
      ast = quote do: fn <<x::8>> when Bitwise.bnot(x) != 0 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 1, :bnot, 0, nil} = Enum.at(ssa.ops, 1)
    end

    test "compiles bitwise with two bindings" do
      ast = quote do: fn <<x::8, y::8>> when Bitwise.band(x, y) > 0 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:alu, 2, :band, 0, 1} = Enum.at(ssa.ops, 2)
    end
  end

  describe "from_clause/2 - actions" do
    test "compiles accept action" do
      ast = quote do: fn <<_::8>> -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:ret, :accept} = List.last(ssa.ops)
    end

    test "compiles reject action" do
      ast = quote do: fn <<_::8>> -> false end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:ret, :reject} = List.last(ssa.ops)
    end

    test "compiles numeric action" do
      ast = quote do: fn <<_::8>> -> 42 end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:ret, {:imm, 42}} = List.last(ssa.ops)
    end

    test "compiles binding return" do
      ast = quote do: fn <<x::8, _::binary>> -> x end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert {:ret, 0} = List.last(ssa.ops)
    end

    test "compiles expression return" do
      ast = quote do: fn <<x::8, y::8, _::binary>> -> x + y end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Should have ALU op then ret with result vreg
      assert {:alu, 2, :add, 0, 1} = Enum.at(ssa.ops, 2)
      assert {:ret, 2} = List.last(ssa.ops)
    end

    test "compiles condition return" do
      ast = quote do: fn <<x::8, y::8, _::binary>> -> x == y end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Should have comparison with local reject label
      ops = ssa.ops

      cmp_op =
        Enum.find(ops, fn
          {:cmp, :eq, _, _, _} -> true
          _ -> false
        end)

      assert cmp_op != nil
      {:cmp, :eq, _, _, reject_label} = cmp_op
      # Reject label should be internal, not :fail
      assert reject_label != :fail
    end
  end

  describe "from_clause/2 - no guard" do
    test "compiles clause without guard" do
      ast = quote do: fn <<42::8>> -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Should just have literal check and return
      assert [
               {:load_packet, 0, :byte, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 42}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end
  end

  describe "operations/1" do
    test "returns the ops list" do
      ast = quote do: fn <<x::8>> when x == 42 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert SSA.operations(ssa) == ssa.ops
    end
  end
end
