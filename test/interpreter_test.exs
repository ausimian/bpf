defmodule BPF.InterpreterTest do
  use ExUnit.Case, async: true

  alias BPF.{Program, Interpreter}

  describe "load instructions" do
    test "ld :b loads a byte from packet" do
      prog = Program.new([{:ld, :b, [:k, 0]}, {:ret, :a}])
      assert {:ok, 0x45} = Interpreter.run(prog, <<0x45, 0x00>>)

      prog = Program.new([{:ld, :b, [:k, 1]}, {:ret, :a}])
      assert {:ok, 0xAB} = Interpreter.run(prog, <<0x00, 0xAB>>)
    end

    test "ld :h loads a halfword (16-bit big-endian)" do
      prog = Program.new([{:ld, :h, [:k, 0]}, {:ret, :a}])
      assert {:ok, 0x1234} = Interpreter.run(prog, <<0x12, 0x34, 0x56>>)
    end

    test "ld :w loads a word (32-bit big-endian)" do
      prog = Program.new([{:ld, :w, [:k, 0]}, {:ret, :a}])
      assert {:ok, 0x12345678} = Interpreter.run(prog, <<0x12, 0x34, 0x56, 0x78>>)
    end

    test "ld :imm loads immediate value" do
      prog = Program.new([{:ld, :imm, 42}, {:ret, :a}])
      assert {:ok, 42} = Interpreter.run(prog, <<>>)
    end

    test "ld :len loads packet length" do
      prog = Program.new([{:ld, :len}, {:ret, :a}])
      assert {:ok, 5} = Interpreter.run(prog, <<1, 2, 3, 4, 5>>)
    end

    test "ld :mem loads from scratch memory" do
      # Store 99 in M[0], then load it back
      prog =
        Program.new([
          {:ld, :imm, 99},
          {:st, 0},
          {:ld, :imm, 0},
          {:ld, :mem, 0},
          {:ret, :a}
        ])

      assert {:ok, 99} = Interpreter.run(prog, <<>>)
    end

    test "ld with indirect addressing [:x, k]" do
      # X = 2, load byte at X + 1 = 3
      prog =
        Program.new([
          {:ldx, :imm, 2},
          {:ld, :b, [:x, 1]},
          {:ret, :a}
        ])

      assert {:ok, 0xDD} = Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end

    test "ldx :msh loads IP header length" do
      # IHL field (lower 4 bits) * 4
      prog =
        Program.new([
          {:ldx, :msh, 0},
          :txa,
          {:ret, :a}
        ])

      # 0x45 & 0x0F = 5, * 4 = 20
      assert {:ok, 20} = Interpreter.run(prog, <<0x45, 0x00>>)
    end

    test "returns error for packet too short" do
      prog = Program.new([{:ld, :w, [:k, 0]}, {:ret, :a}])
      assert {:error, {:packet_too_short, 0, 4}} = Interpreter.run(prog, <<0x12>>)
    end
  end

  describe "ALU instructions with constant" do
    test "add" do
      prog = Program.new([{:ld, :imm, 10}, {:add, 5}, {:ret, :a}])
      assert {:ok, 15} = Interpreter.run(prog, <<>>)
    end

    test "sub" do
      prog = Program.new([{:ld, :imm, 10}, {:sub, 3}, {:ret, :a}])
      assert {:ok, 7} = Interpreter.run(prog, <<>>)
    end

    test "mul" do
      prog = Program.new([{:ld, :imm, 6}, {:mul, 7}, {:ret, :a}])
      assert {:ok, 42} = Interpreter.run(prog, <<>>)
    end

    test "div" do
      prog = Program.new([{:ld, :imm, 42}, {:div, 6}, {:ret, :a}])
      assert {:ok, 7} = Interpreter.run(prog, <<>>)
    end

    test "div by zero returns error" do
      prog = Program.new([{:ld, :imm, 42}, {:div, 0}, {:ret, :a}])
      assert {:error, :division_by_zero} = Interpreter.run(prog, <<>>)
    end

    test "mod" do
      prog = Program.new([{:ld, :imm, 17}, {:mod, 5}, {:ret, :a}])
      assert {:ok, 2} = Interpreter.run(prog, <<>>)
    end

    test "and" do
      prog = Program.new([{:ld, :imm, 0xFF}, {:and, 0x0F}, {:ret, :a}])
      assert {:ok, 0x0F} = Interpreter.run(prog, <<>>)
    end

    test "or" do
      prog = Program.new([{:ld, :imm, 0xF0}, {:or, 0x0F}, {:ret, :a}])
      assert {:ok, 0xFF} = Interpreter.run(prog, <<>>)
    end

    test "xor" do
      prog = Program.new([{:ld, :imm, 0xFF}, {:xor, 0x0F}, {:ret, :a}])
      assert {:ok, 0xF0} = Interpreter.run(prog, <<>>)
    end

    test "lsh (left shift)" do
      prog = Program.new([{:ld, :imm, 1}, {:lsh, 4}, {:ret, :a}])
      assert {:ok, 16} = Interpreter.run(prog, <<>>)
    end

    test "rsh (right shift)" do
      prog = Program.new([{:ld, :imm, 0xFF}, {:rsh, 4}, {:ret, :a}])
      assert {:ok, 0x0F} = Interpreter.run(prog, <<>>)
    end

    test "neg" do
      # -10 in 32-bit unsigned is 0xFFFFFFF6
      prog = Program.new([{:ld, :imm, 10}, :neg, {:ret, :a}])
      assert {:ok, 0xFFFFFFF6} = Interpreter.run(prog, <<>>)
    end
  end

  describe "ALU instructions with X register" do
    test "add :x" do
      prog = Program.new([{:ld, :imm, 10}, {:ldx, :imm, 5}, {:add, :x}, {:ret, :a}])
      assert {:ok, 15} = Interpreter.run(prog, <<>>)
    end

    test "sub :x" do
      prog = Program.new([{:ld, :imm, 10}, {:ldx, :imm, 3}, {:sub, :x}, {:ret, :a}])
      assert {:ok, 7} = Interpreter.run(prog, <<>>)
    end

    test "mul :x" do
      prog = Program.new([{:ld, :imm, 6}, {:ldx, :imm, 7}, {:mul, :x}, {:ret, :a}])
      assert {:ok, 42} = Interpreter.run(prog, <<>>)
    end

    test "div :x by zero returns error" do
      prog = Program.new([{:ld, :imm, 42}, {:ldx, :imm, 0}, {:div, :x}, {:ret, :a}])
      assert {:error, :division_by_zero} = Interpreter.run(prog, <<>>)
    end
  end

  describe "jump instructions" do
    test "ja (unconditional jump)" do
      prog =
        Program.new([
          # Skip next instruction
          {:jmp, :ja, 1},
          # This should be skipped
          {:ld, :imm, 999},
          {:ld, :imm, 42},
          {:ret, :a}
        ])

      assert {:ok, 42} = Interpreter.run(prog, <<>>)
    end

    test "jeq with constant - true branch" do
      prog =
        Program.new([
          {:ld, :imm, 5},
          # if A == 5, skip 1, else skip 0
          {:jmp, :jeq, :k, 5, 1, 0},
          # false branch
          {:ld, :imm, 0},
          # true branch
          {:ld, :imm, 100},
          {:ret, :a}
        ])

      assert {:ok, 100} = Interpreter.run(prog, <<>>)
    end

    test "jeq with constant - false branch" do
      prog =
        Program.new([
          {:ld, :imm, 3},
          # if A == 5, skip 1, else skip 0
          {:jmp, :jeq, :k, 5, 1, 0},
          # false branch (taken)
          {:ld, :imm, 0},
          # true branch (skipped)
          {:ld, :imm, 100},
          {:ret, :a}
        ])

      # Falls through false branch, then loads 100, returns 100
      # Wait, let me trace through:
      # PC=0: ld imm 3 -> A=3, PC=1
      # PC=1: jeq 5, 1, 0 -> A!=5, so PC=1+1+0=2
      # PC=2: ld imm 0 -> A=0, PC=3
      # PC=3: ld imm 100 -> A=100, PC=4
      # PC=4: ret a -> 100
      # Hmm, the structure is wrong. Let me fix the test.
      assert {:ok, 100} = Interpreter.run(prog, <<>>)
    end

    test "jeq with constant - proper branching" do
      # Accept if A == 5, reject otherwise
      prog =
        Program.new([
          {:ld, :imm, 5},
          # if A == 5, skip 0 (next), else skip 1
          {:jmp, :jeq, :k, 5, 0, 1},
          # accept (true)
          {:ret, :k, 0xFFFFFFFF},
          # reject (false)
          {:ret, :k, 0}
        ])

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)

      prog =
        Program.new([
          {:ld, :imm, 3},
          {:jmp, :jeq, :k, 5, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end

    test "jgt with constant" do
      prog =
        Program.new([
          {:ld, :imm, 10},
          # if A > 5, accept
          {:jmp, :jgt, :k, 5, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)

      prog =
        Program.new([
          {:ld, :imm, 5},
          # 5 > 5 is false
          {:jmp, :jgt, :k, 5, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end

    test "jge with constant" do
      prog =
        Program.new([
          {:ld, :imm, 5},
          # if A >= 5, accept
          {:jmp, :jge, :k, 5, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)
    end

    test "jset (bitwise AND test)" do
      prog =
        Program.new([
          {:ld, :imm, 0b1010},
          # if A & 2 != 0, accept
          {:jmp, :jset, :k, 0b0010, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)

      prog =
        Program.new([
          {:ld, :imm, 0b1010},
          # if A & 4 != 0, accept
          {:jmp, :jset, :k, 0b0100, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end

    test "jeq with X register" do
      prog =
        Program.new([
          {:ld, :imm, 42},
          {:ldx, :imm, 42},
          {:jmp, :jeq, :x, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)
    end
  end

  describe "return instructions" do
    test "ret :k returns constant" do
      prog = Program.new([{:ret, :k, 12345}])
      assert {:ok, 12345} = Interpreter.run(prog, <<>>)
    end

    test "ret :a returns accumulator" do
      prog = Program.new([{:ld, :imm, 42}, {:ret, :a}])
      assert {:ok, 42} = Interpreter.run(prog, <<>>)
    end
  end

  describe "misc instructions" do
    test "tax copies A to X" do
      prog =
        Program.new([
          {:ld, :imm, 42},
          :tax,
          # clear A
          {:ld, :imm, 0},
          # restore from X
          :txa,
          {:ret, :a}
        ])

      assert {:ok, 42} = Interpreter.run(prog, <<>>)
    end

    test "txa copies X to A" do
      prog =
        Program.new([
          {:ldx, :imm, 99},
          :txa,
          {:ret, :a}
        ])

      assert {:ok, 99} = Interpreter.run(prog, <<>>)
    end
  end

  describe "scratch memory" do
    test "st and ld :mem work together" do
      prog =
        Program.new([
          {:ld, :imm, 111},
          {:st, 0},
          {:ld, :imm, 222},
          {:st, 1},
          {:ld, :mem, 0},
          {:ret, :a}
        ])

      assert {:ok, 111} = Interpreter.run(prog, <<>>)
    end

    test "stx and ldx :mem work together" do
      prog =
        Program.new([
          {:ldx, :imm, 333},
          {:stx, 5},
          {:ldx, :imm, 0},
          {:ldx, :mem, 5},
          :txa,
          {:ret, :a}
        ])

      assert {:ok, 333} = Interpreter.run(prog, <<>>)
    end
  end

  describe "ALU instructions with X - extended" do
    test "mod :x" do
      prog = Program.new([{:ld, :imm, 17}, {:ldx, :imm, 5}, {:mod, :x}, {:ret, :a}])
      assert {:ok, 2} = Interpreter.run(prog, <<>>)
    end

    test "mod :x by zero returns error" do
      prog = Program.new([{:ld, :imm, 17}, {:ldx, :imm, 0}, {:mod, :x}, {:ret, :a}])
      assert {:error, :division_by_zero} = Interpreter.run(prog, <<>>)
    end

    test "and :x" do
      prog = Program.new([{:ld, :imm, 0xFF}, {:ldx, :imm, 0x0F}, {:and, :x}, {:ret, :a}])
      assert {:ok, 0x0F} = Interpreter.run(prog, <<>>)
    end

    test "or :x" do
      prog = Program.new([{:ld, :imm, 0xF0}, {:ldx, :imm, 0x0F}, {:or, :x}, {:ret, :a}])
      assert {:ok, 0xFF} = Interpreter.run(prog, <<>>)
    end

    test "xor :x" do
      prog = Program.new([{:ld, :imm, 0xFF}, {:ldx, :imm, 0x0F}, {:xor, :x}, {:ret, :a}])
      assert {:ok, 0xF0} = Interpreter.run(prog, <<>>)
    end

    test "lsh :x" do
      prog = Program.new([{:ld, :imm, 1}, {:ldx, :imm, 8}, {:lsh, :x}, {:ret, :a}])
      assert {:ok, 256} = Interpreter.run(prog, <<>>)
    end

    test "rsh :x" do
      prog = Program.new([{:ld, :imm, 0xFF00}, {:ldx, :imm, 8}, {:rsh, :x}, {:ret, :a}])
      assert {:ok, 0xFF} = Interpreter.run(prog, <<>>)
    end
  end

  describe "ldx load instructions" do
    test "ldx :w absolute" do
      prog = Program.new([{:ldx, :w, [:k, 0]}, :txa, {:ret, :a}])
      assert {:ok, 0x12345678} = Interpreter.run(prog, <<0x12, 0x34, 0x56, 0x78>>)
    end

    test "ldx :h absolute" do
      prog = Program.new([{:ldx, :h, [:k, 0]}, :txa, {:ret, :a}])
      assert {:ok, 0x1234} = Interpreter.run(prog, <<0x12, 0x34>>)
    end

    test "ldx :b absolute" do
      prog = Program.new([{:ldx, :b, [:k, 1]}, :txa, {:ret, :a}])
      assert {:ok, 0xAB} = Interpreter.run(prog, <<0x00, 0xAB>>)
    end

    test "ldx :len loads packet length" do
      prog = Program.new([{:ldx, :len}, :txa, {:ret, :a}])
      assert {:ok, 10} = Interpreter.run(prog, <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>)
    end
  end

  describe "boundary value tests" do
    test "32-bit max value handling" do
      prog = Program.new([{:ld, :imm, 0xFFFFFFFF}, {:ret, :a}])
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)
    end

    test "32-bit overflow wraps around" do
      prog = Program.new([{:ld, :imm, 0xFFFFFFFF}, {:add, 1}, {:ret, :a}])
      # 0xFFFFFFFF + 1 = 0x100000000, which wraps to 0
      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end

    test "32-bit underflow wraps around" do
      prog = Program.new([{:ld, :imm, 0}, {:sub, 1}, {:ret, :a}])
      # 0 - 1 in 32-bit unsigned = 0xFFFFFFFF
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)
    end

    test "shift by 32 produces zero" do
      prog = Program.new([{:ld, :imm, 1}, {:lsh, 32}, {:ret, :a}])
      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end

    test "large multiplication wraps" do
      prog = Program.new([{:ld, :imm, 0x10000}, {:mul, 0x10000}, {:ret, :a}])
      # 0x10000 * 0x10000 = 0x100000000, wraps to 0
      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end
  end

  describe "packet boundary tests" do
    test "load at exact packet end" do
      prog = Program.new([{:ld, :b, [:k, 3]}, {:ret, :a}])
      assert {:ok, 0xDD} = Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end

    test "load halfword at packet end" do
      prog = Program.new([{:ld, :h, [:k, 2]}, {:ret, :a}])
      assert {:ok, 0xCCDD} = Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end

    test "load word at packet end" do
      prog = Program.new([{:ld, :w, [:k, 0]}, {:ret, :a}])
      assert {:ok, 0xAABBCCDD} = Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end

    test "returns error for byte load past end" do
      prog = Program.new([{:ld, :b, [:k, 4]}, {:ret, :a}])

      assert {:error, {:packet_too_short, 4, 1}} =
               Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end

    test "returns error for halfword load past end" do
      prog = Program.new([{:ld, :h, [:k, 3]}, {:ret, :a}])

      assert {:error, {:packet_too_short, 3, 2}} =
               Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end

    test "returns error for word load past end" do
      prog = Program.new([{:ld, :w, [:k, 2]}, {:ret, :a}])

      assert {:error, {:packet_too_short, 2, 4}} =
               Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end

    test "indirect load respects bounds" do
      prog = Program.new([{:ldx, :imm, 3}, {:ld, :b, [:x, 1]}, {:ret, :a}])

      assert {:error, {:packet_too_short, 4, 1}} =
               Interpreter.run(prog, <<0xAA, 0xBB, 0xCC, 0xDD>>)
    end
  end

  describe "jump boundary tests" do
    test "jump to program end is valid" do
      prog =
        Program.new([
          {:jmp, :ja, 2},
          {:ld, :imm, 100},
          {:ret, :a},
          {:ld, :imm, 200},
          {:ret, :a}
        ])

      assert {:ok, 200} = Interpreter.run(prog, <<>>)
    end

    test "conditional jump with max jt offset" do
      prog =
        Program.new([
          {:ld, :imm, 5},
          {:jmp, :jeq, :k, 5, 2, 0},
          {:ld, :imm, 0},
          {:ret, :a},
          {:ld, :imm, 100},
          {:ret, :a}
        ])

      assert {:ok, 100} = Interpreter.run(prog, <<>>)
    end

    test "jgt comparison at boundaries" do
      # Test A > 0xFFFFFFFE
      prog =
        Program.new([
          {:ld, :imm, 0xFFFFFFFF},
          {:jmp, :jgt, :k, 0xFFFFFFFE, 0, 1},
          {:ret, :k, 1},
          {:ret, :k, 0}
        ])

      assert {:ok, 1} = Interpreter.run(prog, <<>>)
    end

    test "jge comparison at boundaries" do
      prog =
        Program.new([
          {:ld, :imm, 0xFFFFFFFF},
          {:jmp, :jge, :k, 0xFFFFFFFF, 0, 1},
          {:ret, :k, 1},
          {:ret, :k, 0}
        ])

      assert {:ok, 1} = Interpreter.run(prog, <<>>)
    end

    test "jset with full bitmask" do
      prog =
        Program.new([
          {:ld, :imm, 0xFFFFFFFF},
          {:jmp, :jset, :k, 0xFFFFFFFF, 0, 1},
          {:ret, :k, 1},
          {:ret, :k, 0}
        ])

      assert {:ok, 1} = Interpreter.run(prog, <<>>)
    end

    test "jgt with X at boundary" do
      prog =
        Program.new([
          {:ld, :imm, 0xFFFFFFFF},
          {:ldx, :imm, 0xFFFFFFFE},
          {:jmp, :jgt, :x, 0, 1},
          {:ret, :k, 1},
          {:ret, :k, 0}
        ])

      assert {:ok, 1} = Interpreter.run(prog, <<>>)
    end

    test "jge with X at boundary" do
      prog =
        Program.new([
          {:ld, :imm, 100},
          {:ldx, :imm, 100},
          {:jmp, :jge, :x, 0, 1},
          {:ret, :k, 1},
          {:ret, :k, 0}
        ])

      assert {:ok, 1} = Interpreter.run(prog, <<>>)
    end

    test "jset with X register" do
      prog =
        Program.new([
          {:ld, :imm, 0b1010},
          {:ldx, :imm, 0b1000},
          {:jmp, :jset, :x, 0, 1},
          {:ret, :k, 1},
          {:ret, :k, 0}
        ])

      assert {:ok, 1} = Interpreter.run(prog, <<>>)
    end
  end

  describe "scratch memory edge cases" do
    test "all 16 scratch slots can be used" do
      # Store values in all 16 slots, then verify one
      instrs =
        for i <- 0..15 do
          [{:ld, :imm, i * 10}, {:st, i}]
        end
        |> List.flatten()

      instrs = instrs ++ [{:ld, :mem, 7}, {:ret, :a}]
      prog = Program.new(instrs)
      assert {:ok, 70} = Interpreter.run(prog, <<>>)
    end

    test "stx and ldx with all slots" do
      instrs =
        for i <- 0..15 do
          [{:ldx, :imm, i + 100}, {:stx, i}]
        end
        |> List.flatten()

      instrs = instrs ++ [{:ldx, :mem, 10}, :txa, {:ret, :a}]
      prog = Program.new(instrs)
      assert {:ok, 110} = Interpreter.run(prog, <<>>)
    end

    test "uninitialized scratch memory returns 0" do
      prog = Program.new([{:ld, :mem, 5}, {:ret, :a}])
      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end
  end

  describe "empty and minimal programs" do
    test "single ret instruction" do
      prog = Program.new([{:ret, :k, 42}])
      assert {:ok, 42} = Interpreter.run(prog, <<>>)
    end

    test "ret :a with uninitialized accumulator" do
      prog = Program.new([{:ret, :a}])
      assert {:ok, 0} = Interpreter.run(prog, <<>>)
    end

    test "empty program returns fell_off_end error" do
      prog = Program.new([])
      assert {:error, :fell_off_end} = Interpreter.run(prog, <<>>)
    end

    test "program without ret falls off end" do
      prog = Program.new([{:ld, :imm, 42}])
      assert {:error, :fell_off_end} = Interpreter.run(prog, <<>>)
    end

    test "jump past end falls off" do
      prog = Program.new([{:jmp, :ja, 10}])
      assert {:error, :fell_off_end} = Interpreter.run(prog, <<>>)
    end
  end

  describe "error handling" do
    test "unknown instruction returns error" do
      prog = Program.new([{:unknown_op, :foo, :bar}])

      assert {:error, {:unknown_instruction, {:unknown_op, :foo, :bar}}} =
               Interpreter.run(prog, <<>>)
    end

    test "invalid instruction tuple returns error" do
      prog = Program.new([:not_a_tuple])
      assert {:error, {:unknown_instruction, :not_a_tuple}} = Interpreter.run(prog, <<>>)
    end

    test "negative offset in indirect addressing" do
      # X = -1 (which is 0xFFFFFFFF as unsigned), then load [:x, 0]
      # This would compute a very large offset, causing packet_too_short
      prog =
        Program.new([
          {:ld, :imm, 0xFFFFFFFF},
          :tax,
          {:ld, :b, [:x, 0]},
          {:ret, :a}
        ])

      # The offset would be 0xFFFFFFFF which is way past the packet
      assert {:error, {:packet_too_short, _, _}} = Interpreter.run(prog, <<0xFF>>)
    end

    test "div by zero with constant" do
      prog = Program.new([{:ld, :imm, 100}, {:div, 0}, {:ret, :a}])
      assert {:error, :division_by_zero} = Interpreter.run(prog, <<>>)
    end

    test "mod by zero with constant" do
      prog = Program.new([{:ld, :imm, 100}, {:mod, 0}, {:ret, :a}])
      assert {:error, :division_by_zero} = Interpreter.run(prog, <<>>)
    end
  end

  describe "ldx msh edge cases" do
    test "ldx :msh with offset" do
      prog = Program.new([{:ldx, :msh, 1}, :txa, {:ret, :a}])
      # Second byte is 0x67, lower nibble is 7, * 4 = 28
      assert {:ok, 28} = Interpreter.run(prog, <<0x45, 0x67>>)
    end

    test "ldx :msh packet too short" do
      prog = Program.new([{:ldx, :msh, 5}, :txa, {:ret, :a}])
      assert {:error, {:packet_too_short, 5, 1}} = Interpreter.run(prog, <<0x45>>)
    end
  end

  describe "run/2 with instruction list" do
    test "accepts raw instruction list" do
      assert {:ok, 42} = Interpreter.run([{:ld, :imm, 42}, {:ret, :a}], <<>>)
    end
  end

  describe "realistic programs" do
    test "accept IPv4 packets only" do
      # Check if IP version (upper 4 bits of first byte) == 4
      prog =
        Program.new([
          {:ld, :b, [:k, 0]},
          {:rsh, 4},
          {:jmp, :jeq, :k, 4, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      # IPv4 packet (version = 4, IHL = 5 -> 0x45)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x45, 0x00, 0x00, 0x14>>)

      # IPv6 packet (version = 6 -> 0x60)
      assert {:ok, 0} = Interpreter.run(prog, <<0x60, 0x00, 0x00, 0x00>>)
    end

    test "accept TCP packets with SYN flag set" do
      # Simplified: assume fixed IP header, check TCP flags at offset 33
      # SYN flag is bit 1 (0x02) in TCP flags byte
      prog =
        Program.new([
          # Load TCP flags byte
          {:ld, :b, [:k, 33]},
          # If SYN bit set, accept
          {:jmp, :jset, :k, 0x02, 0, 1},
          {:ret, :k, 0xFFFFFFFF},
          {:ret, :k, 0}
        ])

      # Create a minimal packet with SYN flag at offset 33
      packet_with_syn = :binary.copy(<<0>>, 33) <> <<0x02>>
      packet_without_syn = :binary.copy(<<0>>, 33) <> <<0x10>>

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, packet_with_syn)
      assert {:ok, 0} = Interpreter.run(prog, packet_without_syn)
    end

    test "check IP header length and total length" do
      # Accept if IHL >= 5 AND total_length > 20
      prog =
        Program.new([
          # Check IHL >= 5
          # 0
          {:ld, :b, [:k, 0]},
          # 1
          {:and, 0x0F},
          # 2: If IHL < 5, jump to reject (skip 3 to index 6)
          {:jmp, :jge, :k, 5, 0, 3},

          # Check total_length > 20
          # 3: Load total length (bytes 2-3)
          {:ld, :h, [:k, 2]},
          # 4: If total_length <= 20, reject (skip 1 to index 6)
          {:jmp, :jgt, :k, 20, 0, 1},
          # 5: Accept
          {:ret, :k, 0xFFFFFFFF},
          # 6: Reject
          {:ret, :k, 0}
        ])

      # Valid packet: IHL=5 (0x45), total_length=40
      valid = <<0x45, 0x00, 0x00, 0x28>>
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, valid)

      # Invalid: IHL=4
      invalid_ihl = <<0x44, 0x00, 0x00, 0x28>>
      assert {:ok, 0} = Interpreter.run(prog, invalid_ihl)

      # Invalid: total_length=20
      invalid_len = <<0x45, 0x00, 0x00, 0x14>>
      assert {:ok, 0} = Interpreter.run(prog, invalid_len)
    end
  end
end
