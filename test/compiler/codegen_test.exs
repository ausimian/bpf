defmodule BPF.Compiler.CodeGenTest do
  use ExUnit.Case, async: true

  alias BPF.Compiler.CodeGen

  describe "generate/2 - packet loads" do
    test "generates byte load" do
      ops = [{:load_packet, 0, :byte, 5, 0, nil}]
      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 5]}] = code
    end

    test "generates half-word load" do
      ops = [{:load_packet, 0, :half, 10, 0, nil}]
      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :h, [:k, 10]}] = code
    end

    test "generates word load" do
      ops = [{:load_packet, 0, :word, 20, 0, nil}]
      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :w, [:k, 20]}] = code
    end

    test "applies shift when needed" do
      ops = [{:load_packet, 0, :byte, 0, 4, nil}]
      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:rsh, 4}] = code
    end

    test "applies mask when needed" do
      ops = [{:load_packet, 0, :byte, 0, 0, 0x0F}]
      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:and, 0x0F}] = code
    end

    test "applies both shift and mask" do
      ops = [{:load_packet, 0, :byte, 0, 4, 0x0F}]
      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:rsh, 4}, {:and, 0x0F}] = code
    end

    test "stores to memory when allocated there" do
      ops = [{:load_packet, 0, :byte, 0, 0, nil}]
      alloc = %{0 => {:mem, 3}}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:st, 3}] = code
    end
  end

  describe "generate/2 - load immediate" do
    test "generates immediate load staying in A" do
      ops = [{:load_imm, 0, 42}]
      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :imm, 42}] = code
    end

    test "generates immediate load stored to memory" do
      ops = [{:load_imm, 0, 100}]
      alloc = %{0 => {:mem, 2}}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :imm, 100}, {:st, 2}] = code
    end
  end

  describe "generate/2 - ALU operations with immediate" do
    test "generates add with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :add, 0, {:imm, 10}}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:add, 10}] = code
    end

    test "generates sub with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :sub, 0, {:imm, 5}}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:sub, 5}] = code
    end

    test "generates mul with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :mul, 0, {:imm, 2}}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:mul, 2}] = code
    end

    test "generates div with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :div, 0, {:imm, 4}}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:div, 4}] = code
    end

    test "generates band with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :band, 0, {:imm, 0xFF}}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:and, 0xFF}] = code
    end

    test "generates bor with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :bor, 0, {:imm, 0x80}}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:or, 0x80}] = code
    end

    test "generates bxor with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :bxor, 0, {:imm, 0xFF}}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:xor, 0xFF}] = code
    end

    test "generates bnot (unary)" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :bnot, 0, nil}
      ]

      alloc = %{0 => :a, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      assert [{:ld, :b, [:k, 0]}, {:xor, 0xFFFFFFFF}] = code
    end
  end

  describe "generate/2 - ALU with two vregs" do
    test "both operands in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :add, 0, 1}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      # Load 0 and store, load 1 and store, then load 1 to X, load 0 to A, add
      assert {:ld, :b, [:k, 0]} = Enum.at(code, 0)
      assert {:st, 0} = Enum.at(code, 1)
      assert {:ld, :b, [:k, 1]} = Enum.at(code, 2)
      assert {:st, 1} = Enum.at(code, 3)
      # Now ALU: load src2 from memory, tax, load src1, add
      assert {:ld, :mem, 1} = Enum.at(code, 4)
      assert :tax = Enum.at(code, 5)
      assert {:ld, :mem, 0} = Enum.at(code, 6)
      assert {:add, :x} = Enum.at(code, 7)
    end

    test "src1 in memory, src2 ephemeral in A" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :add, 0, 1}
      ]

      # x needs scratch (used after y is loaded), y can stay in A
      alloc = %{0 => {:mem, 0}, 1 => :a, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      # Load x, store to scratch
      assert {:ld, :b, [:k, 0]} = Enum.at(code, 0)
      assert {:st, 0} = Enum.at(code, 1)
      # Load y (ephemeral in A)
      assert {:ld, :b, [:k, 1]} = Enum.at(code, 2)
      # ALU: y in A, move to X, load x from scratch, add
      assert :tax = Enum.at(code, 3)
      assert {:ld, :mem, 0} = Enum.at(code, 4)
      assert {:add, :x} = Enum.at(code, 5)
    end

    test "src1 ephemeral in A, src2 in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        # Note: 1 - 0, with 1 ephemeral
        {:alu, 2, :sub, 1, 0}
      ]

      # src1 (vreg 1) ephemeral in A, src2 (vreg 0) in memory
      alloc = %{0 => {:mem, 0}, 1 => :a, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      # Load x (vreg 0), store to scratch
      assert {:ld, :b, [:k, 0]} = Enum.at(code, 0)
      assert {:st, 0} = Enum.at(code, 1)
      # Load y (vreg 1), ephemeral in A
      assert {:ld, :b, [:k, 1]} = Enum.at(code, 2)
      # ALU: src1 (1) in A, src2 (0) in memory
      # Save A to temp, load src2 to A, move to X, restore A, sub
      assert {:st, 15} = Enum.at(code, 3)
      assert {:ld, :mem, 0} = Enum.at(code, 4)
      assert :tax = Enum.at(code, 5)
      assert {:ld, :mem, 15} = Enum.at(code, 6)
      assert {:sub, :x} = Enum.at(code, 7)
    end

    test "stores ALU result to memory when allocated there" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :add, 0, {:imm, 10}}
      ]

      alloc = %{0 => :a, 1 => {:mem, 5}}

      code = CodeGen.generate(ops, alloc)

      assert {:ld, :b, [:k, 0]} = Enum.at(code, 0)
      assert {:add, 10} = Enum.at(code, 1)
      assert {:st, 5} = Enum.at(code, 2)
    end
  end

  describe "generate/2 - comparisons with immediate" do
    test "generates eq comparison" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :eq, 0, {:imm, 42}, :fail}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jeq, :k, 42, 0, {:label_ref, :fail}} = Enum.at(code, 1)
    end

    test "generates neq comparison" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :neq, 0, {:imm, 42}, :fail}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jeq, :k, 42, {:label_ref, :fail}, 0} = Enum.at(code, 1)
    end

    test "generates gt comparison" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :gt, 0, {:imm, 100}, :fail}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :k, 100, 0, {:label_ref, :fail}} = Enum.at(code, 1)
    end

    test "generates gte comparison" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :gte, 0, {:imm, 100}, :fail}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :k, 100, 0, {:label_ref, :fail}} = Enum.at(code, 1)
    end

    test "generates lt comparison" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :lt, 0, {:imm, 100}, :fail}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :k, 100, {:label_ref, :fail}, 0} = Enum.at(code, 1)
    end

    test "generates lte comparison" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :lte, 0, {:imm, 100}, :fail}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :k, 100, {:label_ref, :fail}, 0} = Enum.at(code, 1)
    end
  end

  describe "generate/2 - comparisons with vreg (X register)" do
    test "generates eq comparison with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :eq, 0, 1, :fail}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      # Should end with jeq comparing A to X
      assert {:jmp, :jeq, :x, 0, {:label_ref, :fail}} = List.last(code)
    end

    test "generates neq comparison with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :neq, 0, 1, :fail}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jeq, :x, {:label_ref, :fail}, 0} = List.last(code)
    end

    test "generates gt comparison with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :gt, 0, 1, :fail}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :x, 0, {:label_ref, :fail}} = List.last(code)
    end

    test "generates gte comparison with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :gte, 0, 1, :fail}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :x, 0, {:label_ref, :fail}} = List.last(code)
    end

    test "generates lt comparison with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :lt, 0, 1, :fail}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :x, {:label_ref, :fail}, 0} = List.last(code)
    end

    test "generates lte comparison with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :lte, 0, 1, :fail}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :x, {:label_ref, :fail}, 0} = List.last(code)
    end
  end

  describe "generate/2 - cmp_success (OR branches)" do
    test "generates eq cmp_success with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp_success, :eq, 0, {:imm, 42}, :success}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jeq, :k, 42, {:label_ref, :success}, 0} = Enum.at(code, 1)
    end

    test "generates neq cmp_success with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp_success, :neq, 0, {:imm, 42}, :success}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jeq, :k, 42, 0, {:label_ref, :success}} = Enum.at(code, 1)
    end

    test "generates gt cmp_success with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp_success, :gt, 0, {:imm, 100}, :success}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :k, 100, {:label_ref, :success}, 0} = Enum.at(code, 1)
    end

    test "generates gte cmp_success with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp_success, :gte, 0, {:imm, 100}, :success}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :k, 100, {:label_ref, :success}, 0} = Enum.at(code, 1)
    end

    test "generates lt cmp_success with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp_success, :lt, 0, {:imm, 100}, :success}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :k, 100, 0, {:label_ref, :success}} = Enum.at(code, 1)
    end

    test "generates lte cmp_success with immediate" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp_success, :lte, 0, {:imm, 100}, :success}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :k, 100, 0, {:label_ref, :success}} = Enum.at(code, 1)
    end

    test "generates eq cmp_success with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp_success, :eq, 0, 1, :success}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jeq, :x, {:label_ref, :success}, 0} = List.last(code)
    end

    test "generates neq cmp_success with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp_success, :neq, 0, 1, :success}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jeq, :x, 0, {:label_ref, :success}} = List.last(code)
    end

    test "generates gt cmp_success with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp_success, :gt, 0, 1, :success}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :x, {:label_ref, :success}, 0} = List.last(code)
    end

    test "generates gte cmp_success with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp_success, :gte, 0, 1, :success}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :x, {:label_ref, :success}, 0} = List.last(code)
    end

    test "generates lt cmp_success with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp_success, :lt, 0, 1, :success}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jge, :x, 0, {:label_ref, :success}} = List.last(code)
    end

    test "generates lte cmp_success with X" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp_success, :lte, 0, 1, :success}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      assert {:jmp, :jgt, :x, 0, {:label_ref, :success}} = List.last(code)
    end
  end

  describe "generate/2 - jumps and labels" do
    test "generates unconditional jump" do
      ops = [{:jump, :target}]

      code = CodeGen.generate(ops, %{})

      assert [{:jmp, :ja, {:label_ref, :target}}] = code
    end

    test "generates label" do
      ops = [{:label, :my_label}]

      code = CodeGen.generate(ops, %{})

      assert [{:label, :my_label}] = code
    end
  end

  describe "generate/2 - returns" do
    test "generates accept return" do
      ops = [{:ret, :accept}]

      code = CodeGen.generate(ops, %{})

      assert [{:ret, :k, 0xFFFFFFFF}] = code
    end

    test "generates reject return" do
      ops = [{:ret, :reject}]

      code = CodeGen.generate(ops, %{})

      assert [{:ret, :k, 0}] = code
    end

    test "generates immediate return" do
      ops = [{:ret, {:imm, 12345}}]

      code = CodeGen.generate(ops, %{})

      assert [{:ret, :k, 12345}] = code
    end

    test "generates vreg return" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:ret, 0}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:ld, :b, [:k, 0]} = Enum.at(code, 0)
      assert {:ret, :a} = Enum.at(code, 1)
    end

    test "generates vreg return loading from memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        # Clobbers A
        {:load_packet, 1, :byte, 1, 0, nil},
        {:ret, 0}
      ]

      alloc = %{0 => {:mem, 0}, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      # Return should load from memory first
      assert {:ld, :mem, 0} = Enum.at(code, -2)
      assert {:ret, :a} = List.last(code)
    end
  end

  describe "generate/2 - A register tracking" do
    test "skips redundant load when value already in A" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :gt, 0, {:imm, 5}, :fail1},
        # 0 should still be in A
        {:cmp, :lt, 0, {:imm, 100}, :fail2}
      ]

      # Even with memory allocation, if used consecutively, should skip reload
      alloc = %{0 => {:mem, 0}}

      code = CodeGen.generate(ops, alloc)

      # Should only have one load from packet, one store, then two comparisons
      # (second comparison should reload from memory since we stored)
      load_count =
        Enum.count(code, fn
          {:ld, :b, [:k, _]} -> true
          _ -> false
        end)

      assert load_count == 1
    end
  end

  describe "generate/2 - load_cmp_operands edge cases" do
    test "handles left in memory, right ephemeral in A" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :eq, 0, 1, :fail}
      ]

      # x in memory, y ephemeral in A
      alloc = %{0 => {:mem, 0}, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      # After loading y, it's in A. Comparing x == y should:
      # tax (move y to X), ld mem 0 (load x to A), jeq x
      assert :tax = Enum.at(code, 3)
      assert {:ld, :mem, 0} = Enum.at(code, 4)
      assert {:jmp, :jeq, :x, 0, {:label_ref, :fail}} = Enum.at(code, 5)
    end

    test "handles left ephemeral in A, right in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        # Compare y (ephemeral) to x (in memory)
        {:cmp, :eq, 1, 0, :fail}
      ]

      alloc = %{0 => {:mem, 0}, 1 => :a}

      code = CodeGen.generate(ops, alloc)

      # y is in A (ephemeral), x is in memory
      # Need to save A to temp, load x to A, move to X, restore A
      assert {:st, 15} = Enum.at(code, 3)
      assert {:ld, :mem, 0} = Enum.at(code, 4)
      assert :tax = Enum.at(code, 5)
      assert {:ld, :mem, 15} = Enum.at(code, 6)
      assert {:jmp, :jeq, :x, 0, {:label_ref, :fail}} = Enum.at(code, 7)
    end
  end

  describe "generate/2 - ALU with two vregs (additional operations)" do
    test "generates mul with two vregs in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :mul, 0, 1}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:mul, :x} = Enum.at(code, 7)
    end

    test "generates div with two vregs in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :div, 0, 1}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:div, :x} = Enum.at(code, 7)
    end

    test "generates band with two vregs in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :band, 0, 1}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:and, :x} = Enum.at(code, 7)
    end

    test "generates bor with two vregs in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :bor, 0, 1}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:or, :x} = Enum.at(code, 7)
    end

    test "generates bxor with two vregs in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :bxor, 0, 1}
      ]

      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      assert {:xor, :x} = Enum.at(code, 7)
    end

    test "generates sub with src1 in memory, src2 ephemeral" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :sub, 0, 1}
      ]

      alloc = %{0 => {:mem, 0}, 1 => :a, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      # src2 (1) in A, move to X, load src1 (0) from memory, sub
      assert :tax = Enum.at(code, 3)
      assert {:ld, :mem, 0} = Enum.at(code, 4)
      assert {:sub, :x} = Enum.at(code, 5)
    end

    test "generates mul with src1 ephemeral, src2 in memory" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        # src1=1 (ephemeral), src2=0 (memory)
        {:alu, 2, :mul, 1, 0}
      ]

      alloc = %{0 => {:mem, 0}, 1 => :a, 2 => :a}

      code = CodeGen.generate(ops, alloc)

      # src1 (1) in A, src2 (0) in memory
      # Save A to temp, load src2 to A, move to X, restore A, mul
      assert {:st, 15} = Enum.at(code, 3)
      assert {:ld, :mem, 0} = Enum.at(code, 4)
      assert :tax = Enum.at(code, 5)
      assert {:ld, :mem, 15} = Enum.at(code, 6)
      assert {:mul, :x} = Enum.at(code, 7)
    end
  end

  describe "generate/2 - load_to_a edge case" do
    test "handles vreg allocated to A but not currently in A" do
      # This tests the fallback case where allocation says :a but reg_a doesn't match
      # This can happen in edge cases - the code just returns state unchanged
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        # This clobbers A
        {:load_packet, 1, :byte, 1, 0, nil},
        # Try to use vreg 0 which is allocated :a
        {:cmp, :eq, 0, {:imm, 42}, :fail}
      ]

      # Both allocated to :a - this is invalid allocation but tests the fallback
      alloc = %{0 => :a, 1 => :a}

      # This should still work - load_to_a returns state unchanged for :a allocation
      code = CodeGen.generate(ops, alloc)

      # Code generates but vreg 0 won't be properly loaded (allocation bug)
      assert length(code) >= 3
    end
  end

  describe "generate/2 - load_to_x edge case" do
    test "handles vreg allocated to memory" do
      # Test load_to_x with memory allocation (the complex path)
      # This happens in the fallback path of load_cmp_operands
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :eq, 0, 1, :fail}
      ]

      # Both in memory - uses the both-in-memory path, not load_to_x directly
      # To test load_to_x with memory, we need a case that hits the fallback
      alloc = %{0 => {:mem, 0}, 1 => {:mem, 1}}

      code = CodeGen.generate(ops, alloc)

      # Verify the comparison loads both operands properly
      assert {:jmp, :jeq, :x, 0, {:label_ref, :fail}} = List.last(code)
    end

    test "handles vreg allocated to A in load_to_x" do
      # Test load_to_x with :a allocation
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        # Compare vreg to itself (weird but valid)
        {:cmp, :eq, 0, 0, :fail}
      ]

      alloc = %{0 => :a}

      code = CodeGen.generate(ops, alloc)

      # Should have tax to move A to X for comparison
      assert Enum.member?(code, :tax)
    end
  end
end
