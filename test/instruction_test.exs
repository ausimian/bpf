defmodule BPF.InstructionTest do
  use ExUnit.Case, async: true

  alias BPF.Instruction

  describe "encode/1 load instructions into A" do
    test "ld :w absolute" do
      assert {0x20, 0, 0, 100} = Instruction.encode({:ld, :w, [:k, 100]})
    end

    test "ld :h absolute" do
      assert {0x28, 0, 0, 50} = Instruction.encode({:ld, :h, [:k, 50]})
    end

    test "ld :b absolute" do
      assert {0x30, 0, 0, 10} = Instruction.encode({:ld, :b, [:k, 10]})
    end

    test "ld :w indirect" do
      assert {0x40, 0, 0, 4} = Instruction.encode({:ld, :w, [:x, 4]})
    end

    test "ld :h indirect" do
      assert {0x48, 0, 0, 2} = Instruction.encode({:ld, :h, [:x, 2]})
    end

    test "ld :b indirect" do
      assert {0x50, 0, 0, 1} = Instruction.encode({:ld, :b, [:x, 1]})
    end

    test "ld :imm immediate" do
      assert {0x00, 0, 0, 42} = Instruction.encode({:ld, :imm, 42})
    end

    test "ld :len packet length" do
      assert {0x80, 0, 0, 0} = Instruction.encode({:ld, :len})
    end

    test "ld :mem scratch memory" do
      assert {0x60, 0, 0, 5} = Instruction.encode({:ld, :mem, 5})
    end
  end

  describe "encode/1 load instructions into X" do
    test "ldx :w absolute" do
      assert {0x21, 0, 0, 100} = Instruction.encode({:ldx, :w, [:k, 100]})
    end

    test "ldx :h absolute" do
      assert {0x29, 0, 0, 50} = Instruction.encode({:ldx, :h, [:k, 50]})
    end

    test "ldx :b absolute" do
      assert {0x31, 0, 0, 10} = Instruction.encode({:ldx, :b, [:k, 10]})
    end

    test "ldx :imm immediate" do
      assert {0x01, 0, 0, 99} = Instruction.encode({:ldx, :imm, 99})
    end

    test "ldx :len packet length" do
      assert {0x81, 0, 0, 0} = Instruction.encode({:ldx, :len})
    end

    test "ldx :mem scratch memory" do
      assert {0x61, 0, 0, 3} = Instruction.encode({:ldx, :mem, 3})
    end

    test "ldx :msh IP header length" do
      assert {0xB1, 0, 0, 0} = Instruction.encode({:ldx, :msh, 0})
    end
  end

  describe "encode/1 store instructions" do
    test "st stores A to memory" do
      assert {0x02, 0, 0, 7} = Instruction.encode({:st, 7})
    end

    test "stx stores X to memory" do
      assert {0x03, 0, 0, 8} = Instruction.encode({:stx, 8})
    end
  end

  describe "encode/1 ALU instructions with constant" do
    test "add" do
      assert {0x04, 0, 0, 10} = Instruction.encode({:add, 10})
    end

    test "sub" do
      assert {0x14, 0, 0, 5} = Instruction.encode({:sub, 5})
    end

    test "mul" do
      assert {0x24, 0, 0, 3} = Instruction.encode({:mul, 3})
    end

    test "div" do
      assert {0x34, 0, 0, 2} = Instruction.encode({:div, 2})
    end

    test "mod" do
      assert {0x94, 0, 0, 7} = Instruction.encode({:mod, 7})
    end

    test "and" do
      assert {0x54, 0, 0, 0xFF} = Instruction.encode({:and, 0xFF})
    end

    test "or" do
      assert {0x44, 0, 0, 0x0F} = Instruction.encode({:or, 0x0F})
    end

    test "xor" do
      assert {0xA4, 0, 0, 0xAA} = Instruction.encode({:xor, 0xAA})
    end

    test "lsh" do
      assert {0x64, 0, 0, 4} = Instruction.encode({:lsh, 4})
    end

    test "rsh" do
      assert {0x74, 0, 0, 8} = Instruction.encode({:rsh, 8})
    end
  end

  describe "encode/1 ALU instructions with X register" do
    test "add :x" do
      assert {0x0C, 0, 0, 0} = Instruction.encode({:add, :x})
    end

    test "sub :x" do
      assert {0x1C, 0, 0, 0} = Instruction.encode({:sub, :x})
    end

    test "mul :x" do
      assert {0x2C, 0, 0, 0} = Instruction.encode({:mul, :x})
    end

    test "div :x" do
      assert {0x3C, 0, 0, 0} = Instruction.encode({:div, :x})
    end

    test "mod :x" do
      assert {0x9C, 0, 0, 0} = Instruction.encode({:mod, :x})
    end

    test "and :x" do
      assert {0x5C, 0, 0, 0} = Instruction.encode({:and, :x})
    end

    test "or :x" do
      assert {0x4C, 0, 0, 0} = Instruction.encode({:or, :x})
    end

    test "xor :x" do
      assert {0xAC, 0, 0, 0} = Instruction.encode({:xor, :x})
    end

    test "lsh :x" do
      assert {0x6C, 0, 0, 0} = Instruction.encode({:lsh, :x})
    end

    test "rsh :x" do
      assert {0x7C, 0, 0, 0} = Instruction.encode({:rsh, :x})
    end
  end

  describe "encode/1 negation" do
    test "neg" do
      assert {0x84, 0, 0, 0} = Instruction.encode(:neg)
    end
  end

  describe "encode/1 jump instructions" do
    test "ja unconditional" do
      assert {0x05, 0, 0, 10} = Instruction.encode({:jmp, :ja, 10})
    end

    test "jeq with constant" do
      assert {0x15, 2, 3, 100} = Instruction.encode({:jmp, :jeq, :k, 100, 2, 3})
    end

    test "jgt with constant" do
      assert {0x25, 1, 0, 50} = Instruction.encode({:jmp, :jgt, :k, 50, 1, 0})
    end

    test "jge with constant" do
      assert {0x35, 0, 1, 25} = Instruction.encode({:jmp, :jge, :k, 25, 0, 1})
    end

    test "jset with constant" do
      assert {0x45, 1, 2, 0x80} = Instruction.encode({:jmp, :jset, :k, 0x80, 1, 2})
    end

    test "jeq with X" do
      assert {0x1D, 3, 4, 0} = Instruction.encode({:jmp, :jeq, :x, 3, 4})
    end

    test "jgt with X" do
      assert {0x2D, 1, 2, 0} = Instruction.encode({:jmp, :jgt, :x, 1, 2})
    end

    test "jge with X" do
      assert {0x3D, 0, 5, 0} = Instruction.encode({:jmp, :jge, :x, 0, 5})
    end

    test "jset with X" do
      assert {0x4D, 2, 0, 0} = Instruction.encode({:jmp, :jset, :x, 2, 0})
    end
  end

  describe "encode/1 return instructions" do
    test "ret :k constant" do
      assert {0x06, 0, 0, 0xFFFFFFFF} = Instruction.encode({:ret, :k, 0xFFFFFFFF})
    end

    test "ret :k zero" do
      assert {0x06, 0, 0, 0} = Instruction.encode({:ret, :k, 0})
    end

    test "ret :a accumulator" do
      assert {0x16, 0, 0, 0} = Instruction.encode({:ret, :a})
    end
  end

  describe "encode/1 misc instructions" do
    test "tax copies A to X" do
      assert {0x07, 0, 0, 0} = Instruction.encode(:tax)
    end

    test "txa copies X to A" do
      assert {0x87, 0, 0, 0} = Instruction.encode(:txa)
    end
  end

  describe "to_binary/1" do
    test "produces 8 bytes per instruction" do
      binary = Instruction.to_binary({:ld, :b, [:k, 0]})
      assert byte_size(binary) == 8
    end

    test "encodes in little-endian format" do
      # {:ld, :w, [:k, 0]} -> code=0x20, jt=0, jf=0, k=0
      binary = Instruction.to_binary({:ld, :w, [:k, 0]})
      assert <<0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00>> = binary
    end

    test "encodes jump offsets correctly" do
      # {:jmp, :jeq, :k, 5, 2, 3} -> code=0x15, jt=2, jf=3, k=5
      binary = Instruction.to_binary({:jmp, :jeq, :k, 5, 2, 3})
      assert <<0x15, 0x00, 0x02, 0x03, 0x05, 0x00, 0x00, 0x00>> = binary
    end

    test "encodes large k value correctly" do
      # {:ret, :k, 0xFFFFFFFF}
      binary = Instruction.to_binary({:ret, :k, 0xFFFFFFFF})
      assert <<0x06, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF>> = binary
    end

    test "encodes halfword load at offset" do
      # {:ld, :h, [:k, 12]} -> code=0x28, k=12
      binary = Instruction.to_binary({:ld, :h, [:k, 12]})
      assert <<0x28, 0x00, 0x00, 0x00, 0x0C, 0x00, 0x00, 0x00>> = binary
    end
  end
end
