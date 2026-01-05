defmodule BPF.Compiler.SSATest do
  use ExUnit.Case, async: true

  alias BPF.Compiler.{SSA, Liveness, RegAlloc, CodeGen}

  describe "SSA lowering" do
    test "simple binding produces load and compare" do
      ast = quote do: fn <<x::8>> when x == 42 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :byte, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 42}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "sub-byte binding produces load with shift and mask" do
      ast = quote do: fn <<4::4, flags::4>> when flags == 8 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Literal check for 4::4, then binding load for flags::4
      assert [
               # version: shift 4, mask 0xF
               {:load_packet, 0, :byte, 0, 4, 15},
               {:cmp, :eq, 0, {:imm, 4}, :fail},
               # flags: no shift, mask 0xF
               {:load_packet, 1, :byte, 0, 0, 15},
               {:cmp, :eq, 1, {:imm, 8}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "arithmetic produces ALU operation" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      assert [
               {:load_packet, 0, :byte, 0, 0, nil},
               {:load_packet, 1, :byte, 1, 0, nil},
               {:alu, 2, :add, 0, 1},
               {:cmp, :gt, 2, {:imm, 100}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end

    test "unused bindings are not loaded" do
      ast = quote do: fn <<x::8, _y::8>> when x == 42 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)

      # Only x should be loaded, not y
      assert [
               {:load_packet, 0, :byte, 0, 0, nil},
               {:cmp, :eq, 0, {:imm, 42}, :fail},
               {:ret, :accept}
             ] = ssa.ops
    end
  end

  describe "liveness analysis" do
    test "computes live ranges correctly" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)
      liveness = Liveness.analyze(ssa.ops)

      # x defined at 0, used at 2
      assert %{def: 0, last_use: 2} = Map.get(liveness, 0)
      # y defined at 1, used at 2
      assert %{def: 1, last_use: 2} = Map.get(liveness, 1)
      # result defined at 2, used at 3
      assert %{def: 2, last_use: 3} = Map.get(liveness, 2)
    end

    test "identifies multiple uses" do
      ast = quote do: fn <<x::8>> when x > 5 and x < 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)
      liveness = Liveness.analyze(ssa.ops)

      # x is used in two comparisons
      info = Map.get(liveness, 0)
      assert length(info.uses) == 2
    end
  end

  describe "register allocation" do
    test "ephemeral value stays in A" do
      ast = quote do: fn <<x::8>> when x == 42 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)
      liveness = Liveness.analyze(ssa.ops)
      alloc = RegAlloc.allocate(ssa.ops, liveness)

      # x is used immediately - should stay in A
      assert alloc[0] == :a
    end

    test "multi-use value gets scratch slot" do
      ast = quote do: fn <<x::8>> when x > 5 and x < 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)
      liveness = Liveness.analyze(ssa.ops)
      alloc = RegAlloc.allocate(ssa.ops, liveness)

      # x is used twice - needs scratch
      assert {:mem, 0} = alloc[0]
    end

    test "second operand can stay in A for immediate use" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)
      liveness = Liveness.analyze(ssa.ops)
      alloc = RegAlloc.allocate(ssa.ops, liveness)

      # x needs scratch (used after y is loaded)
      assert {:mem, 0} = alloc[0]
      # y can stay in A (used immediately)
      assert alloc[1] == :a
      # result can stay in A
      assert alloc[2] == :a
    end
  end

  describe "code generation" do
    test "generates correct BPF for simple comparison" do
      ast = quote do: fn <<x::8>> when x == 42 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)
      liveness = Liveness.analyze(ssa.ops)
      alloc = RegAlloc.allocate(ssa.ops, liveness)
      code = CodeGen.generate(ssa.ops, alloc)

      assert [
               {:ld, :b, [:k, 0]},
               {:jmp, :jeq, :k, 42, 0, {:label_ref, :fail}},
               {:ret, :k, 0xFFFFFFFF}
             ] = code
    end

    test "generates correct BPF for arithmetic" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      ssa = SSA.from_clause(hd(clauses), :fail)
      liveness = Liveness.analyze(ssa.ops)
      alloc = RegAlloc.allocate(ssa.ops, liveness)
      code = CodeGen.generate(ssa.ops, alloc)

      # Should:
      # 1. Load x, store to scratch
      # 2. Load y (stays in A for ALU)
      # 3. Move y to X, load x from scratch, add
      # 4. Compare result
      assert {:ld, :b, [:k, 0]} = Enum.at(code, 0)
      assert {:st, 0} = Enum.at(code, 1)
      assert {:ld, :b, [:k, 1]} = Enum.at(code, 2)
      assert :tax = Enum.at(code, 3)
      assert {:ld, :mem, 0} = Enum.at(code, 4)
      assert {:add, :x} = Enum.at(code, 5)
    end
  end

  describe "compiler integration" do
    test "produces correct results for simple comparison" do
      ast = quote do: fn <<x::8>> when x == 42 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      {:ok, prog} = BPF.Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = BPF.Interpreter.run(prog, <<42>>)
      assert {:ok, 0} = BPF.Interpreter.run(prog, <<41>>)
    end

    test "produces correct results for AND condition" do
      ast = quote do: fn <<x::8>> when x > 5 and x < 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      {:ok, prog} = BPF.Compiler.compile(clauses)

      # Should reject: 1, 5, 100, 150
      assert {:ok, 0} = BPF.Interpreter.run(prog, <<1>>)
      assert {:ok, 0} = BPF.Interpreter.run(prog, <<5>>)
      assert {:ok, 0} = BPF.Interpreter.run(prog, <<100>>)
      assert {:ok, 0} = BPF.Interpreter.run(prog, <<150>>)

      # Should accept: 6, 50, 99
      assert {:ok, 0xFFFFFFFF} = BPF.Interpreter.run(prog, <<6>>)
      assert {:ok, 0xFFFFFFFF} = BPF.Interpreter.run(prog, <<50>>)
      assert {:ok, 0xFFFFFFFF} = BPF.Interpreter.run(prog, <<99>>)
    end

    test "produces correct results for arithmetic" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      {:ok, prog} = BPF.Compiler.compile(clauses)

      # Should reject: 50+50=100 (not > 100), 10+10=20
      assert {:ok, 0} = BPF.Interpreter.run(prog, <<50, 50>>)
      assert {:ok, 0} = BPF.Interpreter.run(prog, <<10, 10>>)

      # Should accept: 50+51=101, 60+50=110
      assert {:ok, 0xFFFFFFFF} = BPF.Interpreter.run(prog, <<50, 51>>)
      assert {:ok, 0xFFFFFFFF} = BPF.Interpreter.run(prog, <<60, 50>>)
    end

    test "produces efficient code for simple case" do
      ast = quote do: fn <<x::8>> when x == 42 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      {:ok, prog} = BPF.Compiler.compile(clauses)

      # Should produce minimal instructions (load, compare, ret, ret)
      assert length(prog.instructions) <= 5
    end

    test "produces efficient code for arithmetic" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = BPF.Parser.parse(ast)
      {:ok, prog} = BPF.Compiler.compile(clauses)

      # Should produce reasonably efficient code
      assert length(prog.instructions) <= 10
    end
  end
end
