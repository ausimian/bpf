defmodule BPF.ProgramTest do
  use ExUnit.Case, async: true

  alias BPF.Program

  describe "new/2" do
    test "creates program with instructions" do
      prog = Program.new([{:ret, :k, 0}])
      assert %Program{instructions: [{:ret, :k, 0}]} = prog
    end

    test "creates program with bindings" do
      prog = Program.new([{:ret, :k, 0}], %{x: 0, y: 8})
      assert prog.bindings == %{x: 0, y: 8}
    end

    test "creates program with empty bindings by default" do
      prog = Program.new([{:ret, :k, 0}])
      assert prog.bindings == %{}
    end

    test "creates program with empty instructions" do
      prog = Program.new([])
      assert prog.instructions == []
    end
  end

  describe "length/1" do
    test "returns zero for empty program" do
      prog = Program.new([])
      assert Program.length(prog) == 0
    end

    test "returns correct count for single instruction" do
      prog = Program.new([{:ret, :k, 0}])
      assert Program.length(prog) == 1
    end

    test "returns correct count for multiple instructions" do
      prog = Program.new([
        {:ld, :b, [:k, 0]},
        {:jmp, :jeq, :k, 5, 0, 1},
        {:ret, :k, 0xFFFFFFFF},
        {:ret, :k, 0}
      ])
      assert Program.length(prog) == 4
    end

    test "returns correct count for large program" do
      instructions = for i <- 1..100, do: {:ld, :imm, i}
      prog = Program.new(instructions ++ [{:ret, :a}])
      assert Program.length(prog) == 101
    end
  end

  describe "to_bytes/1" do
    test "returns empty binary for empty program" do
      prog = Program.new([])
      assert Program.to_bytes(prog) == <<>>
    end

    test "returns 8 bytes per instruction" do
      prog = Program.new([{:ret, :k, 0}])
      bytes = Program.to_bytes(prog)
      assert byte_size(bytes) == 8
    end

    test "returns 16 bytes for two instructions" do
      prog = Program.new([{:ld, :imm, 42}, {:ret, :a}])
      bytes = Program.to_bytes(prog)
      assert byte_size(bytes) == 16
    end

    test "returns correctly formatted bytecode" do
      # Simple program: load immediate 42, return A
      prog = Program.new([{:ld, :imm, 42}, {:ret, :a}])
      bytes = Program.to_bytes(prog)

      # First instruction: {:ld, :imm, 42} -> code=0x00, k=42
      # Second instruction: {:ret, :a} -> code=0x16
      assert <<
               0x00, 0x00, 0x00, 0x00, 42, 0x00, 0x00, 0x00,
               0x16, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
             >> = bytes
    end

    test "handles large programs" do
      instructions = for _ <- 1..256, do: {:ld, :imm, 0}
      prog = Program.new(instructions)
      bytes = Program.to_bytes(prog)
      assert byte_size(bytes) == 256 * 8
    end

    test "bytecode size is always multiple of 8" do
      for n <- 1..20 do
        instructions = for _ <- 1..n, do: {:ld, :imm, 0}
        prog = Program.new(instructions)
        bytes = Program.to_bytes(prog)
        assert rem(byte_size(bytes), 8) == 0
      end
    end
  end
end
