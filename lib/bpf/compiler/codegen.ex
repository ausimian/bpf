defmodule BPF.Compiler.CodeGen do
  @moduledoc false

  @accept_value 0xFFFFFFFF
  @reject_value 0

  @doc """
  Generate BPF instructions from SSA operations.

  Returns a list of BPF instruction tuples.
  """
  def generate(ops, allocation) do
    state = %{
      allocation: allocation,
      # What's currently in A: nil, {:vreg, n}, {:imm, n}, :unknown
      reg_a: nil,
      instrs: []
    }

    state = Enum.reduce(ops, state, &generate_op/2)

    Enum.reverse(state.instrs)
  end

  # Load from packet
  defp generate_op({:load_packet, vreg, size, offset, shift, mask}, state) do
    alloc = Map.get(state.allocation, vreg)

    # Emit load instruction
    size_atom = size_to_atom(size)
    state = emit(state, {:ld, size_atom, [:k, offset]})

    # Apply shift if needed
    state = if shift > 0, do: emit(state, {:rsh, shift}), else: state

    # Apply mask if needed
    state = if mask, do: emit(state, {:and, mask}), else: state

    # Store to scratch if allocated there
    state =
      case alloc do
        {:mem, slot} ->
          state = emit(state, {:st, slot})
          %{state | reg_a: {:vreg, vreg}}

        :a ->
          %{state | reg_a: {:vreg, vreg}}
      end

    state
  end

  # Load immediate
  defp generate_op({:load_imm, vreg, value}, state) do
    alloc = Map.get(state.allocation, vreg)

    state = emit(state, {:ld, :imm, value})

    case alloc do
      {:mem, slot} ->
        state = emit(state, {:st, slot})
        %{state | reg_a: {:vreg, vreg}}

      :a ->
        %{state | reg_a: {:vreg, vreg}}
    end
  end

  # Load packet length
  defp generate_op({:load_len, vreg}, state) do
    alloc = Map.get(state.allocation, vreg)

    state = emit(state, {:ld, :len})

    case alloc do
      {:mem, slot} ->
        state = emit(state, {:st, slot})
        %{state | reg_a: {:vreg, vreg}}

      :a ->
        %{state | reg_a: {:vreg, vreg}}
    end
  end

  # ALU operation
  defp generate_op({:alu, vreg, op, src1, src2}, state) do
    # Apply operation with src2
    state =
      case src2 do
        {:imm, value} ->
          # Simple case: src1 to A, immediate operation
          state = load_to_a(src1, state)
          emit(state, alu_op(op, value))

        nil when op == :bnot ->
          state = load_to_a(src1, state)
          emit(state, {:xor, 0xFFFFFFFF})

        src2_vreg when is_integer(src2_vreg) ->
          # Complex case: need both operands
          # Strategy: put src2 in X, src1 in A, then operate
          generate_alu_two_vregs(op, src1, src2_vreg, state)
      end

    # Store result if needed
    alloc = Map.get(state.allocation, vreg)

    case alloc do
      {:mem, slot} ->
        state = emit(state, {:st, slot})
        %{state | reg_a: {:vreg, vreg}}

      :a ->
        %{state | reg_a: {:vreg, vreg}}
    end
  end

  # Comparison (jump on failure)
  defp generate_op({:cmp, op, left, right, fail_label}, state) do
    case right do
      {:imm, value} ->
        state = load_to_a(left, state)
        emit(state, cmp_jump(op, :k, value, fail_label))

      right_vreg when is_integer(right_vreg) ->
        # Need both operands in registers
        state = load_cmp_operands(left, right_vreg, state)
        emit(state, cmp_jump(op, :x, 0, fail_label))
    end
  end

  # Comparison (jump on success - for OR branches)
  defp generate_op({:cmp_success, op, left, right, success_label}, state) do
    case right do
      {:imm, value} ->
        state = load_to_a(left, state)
        emit(state, cmp_jump_success(op, :k, value, success_label))

      right_vreg when is_integer(right_vreg) ->
        state = load_cmp_operands(left, right_vreg, state)
        emit(state, cmp_jump_success(op, :x, 0, success_label))
    end
  end

  # Jump
  defp generate_op({:jump, label}, state) do
    emit(state, {:jmp, :ja, {:label_ref, label}})
  end

  # Label
  defp generate_op({:label, name}, state) do
    emit(state, {:label, name})
  end

  # Return
  defp generate_op({:ret, :accept}, state) do
    emit(state, {:ret, :k, @accept_value})
  end

  defp generate_op({:ret, :reject}, state) do
    emit(state, {:ret, :k, @reject_value})
  end

  defp generate_op({:ret, {:imm, value}}, state) do
    emit(state, {:ret, :k, value})
  end

  defp generate_op({:ret, vreg}, state) when is_integer(vreg) do
    state = load_to_a(vreg, state)
    emit(state, {:ret, :a})
  end

  # Load a vreg into A
  defp load_to_a(vreg, state) when is_integer(vreg) do
    if state.reg_a == {:vreg, vreg} do
      # Already in A
      state
    else
      # Need to load from scratch
      alloc = Map.get(state.allocation, vreg)

      case alloc do
        {:mem, slot} ->
          state = emit(state, {:ld, :mem, slot})
          %{state | reg_a: {:vreg, vreg}}

        :a ->
          # It was supposed to stay in A but it's not there
          # This shouldn't happen with correct allocation
          state
      end
    end
  end

  # Load a vreg into X
  defp load_to_x(vreg, state) when is_integer(vreg) do
    alloc = Map.get(state.allocation, vreg)

    case alloc do
      {:mem, slot} ->
        # Save A if needed, load to A, move to X, restore A
        # Use slot 15 as temporary
        state = emit(state, {:st, 15})
        state = emit(state, {:ld, :mem, slot})
        state = emit(state, :tax)
        state = emit(state, {:ld, :mem, 15})
        state

      :a ->
        # Value is in A (or should be), move to X
        # If something else is in A, we have a problem
        emit(state, :tax)
    end
  end

  # Load two operands for comparison: left -> A, right -> X
  defp load_cmp_operands(left, right, state) do
    left_alloc = Map.get(state.allocation, left)
    right_alloc = Map.get(state.allocation, right)

    cond do
      # Both in memory
      match?({:mem, _}, left_alloc) and match?({:mem, _}, right_alloc) ->
        {:mem, left_slot} = left_alloc
        {:mem, right_slot} = right_alloc
        # Load right to X, then left to A
        state = emit(state, {:ld, :mem, right_slot})
        state = emit(state, :tax)
        state = emit(state, {:ld, :mem, left_slot})
        %{state | reg_a: {:vreg, left}}

      # Left in memory, right ephemeral in A
      match?({:mem, _}, left_alloc) and right_alloc == :a ->
        {:mem, left_slot} = left_alloc
        # Right should be in A - move to X, then load left
        state = emit(state, :tax)
        state = emit(state, {:ld, :mem, left_slot})
        %{state | reg_a: {:vreg, left}}

      # Left ephemeral in A, right in memory
      left_alloc == :a and match?({:mem, _}, right_alloc) ->
        {:mem, right_slot} = right_alloc
        # Left is in A - save to temp, load right to A, move to X, restore left
        state = emit(state, {:st, 15})
        state = emit(state, {:ld, :mem, right_slot})
        state = emit(state, :tax)
        state = emit(state, {:ld, :mem, 15})
        %{state | reg_a: {:vreg, left}}

      # Both ephemeral (shouldn't happen with proper allocation)
      true ->
        state = load_to_a(left, state)
        state = load_to_x(right, state)
        state
    end
  end

  # Emit an instruction
  defp emit(state, instr) do
    %{state | instrs: [instr | state.instrs]}
  end

  defp size_to_atom(:byte), do: :b
  defp size_to_atom(:half), do: :h
  defp size_to_atom(:word), do: :w

  defp alu_op(:add, k), do: {:add, k}
  defp alu_op(:sub, k), do: {:sub, k}
  defp alu_op(:mul, k), do: {:mul, k}
  defp alu_op(:div, k), do: {:div, k}
  defp alu_op(:band, k), do: {:and, k}
  defp alu_op(:bor, k), do: {:or, k}
  defp alu_op(:bxor, k), do: {:xor, k}

  defp alu_op_x(:add), do: {:add, :x}
  defp alu_op_x(:sub), do: {:sub, :x}
  defp alu_op_x(:mul), do: {:mul, :x}
  defp alu_op_x(:div), do: {:div, :x}
  defp alu_op_x(:band), do: {:and, :x}
  defp alu_op_x(:bor), do: {:or, :x}
  defp alu_op_x(:bxor), do: {:xor, :x}

  # Handle ALU with two vreg operands
  defp generate_alu_two_vregs(op, src1, src2, state) do
    src1_alloc = Map.get(state.allocation, src1)
    src2_alloc = Map.get(state.allocation, src2)

    cond do
      # Both in memory - load src2 to X, src1 to A
      match?({:mem, _}, src1_alloc) and match?({:mem, _}, src2_alloc) ->
        {:mem, slot2} = src2_alloc
        {:mem, slot1} = src1_alloc
        state = emit(state, {:ld, :mem, slot2})
        state = emit(state, :tax)
        state = emit(state, {:ld, :mem, slot1})
        emit(state, alu_op_x(op))

      # src1 in memory, src2 in A (ephemeral)
      match?({:mem, _}, src1_alloc) and src2_alloc == :a ->
        {:mem, slot1} = src1_alloc
        # src2 should be in A already - move to X, load src1
        state = emit(state, :tax)
        state = emit(state, {:ld, :mem, slot1})
        emit(state, alu_op_x(op))

      # src1 in A, src2 in memory
      src1_alloc == :a and match?({:mem, _}, src2_alloc) ->
        {:mem, slot2} = src2_alloc
        # This is tricky - src1 is ephemeral in A, we need to preserve it
        # Save A to temp, load src2 to A, move to X, restore A
        # Save src1 to temp
        state = emit(state, {:st, 15})
        state = emit(state, {:ld, :mem, slot2})
        # X = src2
        state = emit(state, :tax)
        # A = src1
        state = emit(state, {:ld, :mem, 15})
        emit(state, alu_op_x(op))

      # Both ephemeral in A (shouldn't happen - only one can be in A)
      true ->
        # Fallback - shouldn't reach here with proper allocation
        state = load_to_a(src1, state)
        state = load_to_x(src2, state)
        emit(state, alu_op_x(op))
    end
  end

  # Comparison jumps - fail on false
  defp cmp_jump(:eq, :k, value, fail_label) do
    {:jmp, :jeq, :k, value, 0, {:label_ref, fail_label}}
  end

  defp cmp_jump(:neq, :k, value, fail_label) do
    {:jmp, :jeq, :k, value, {:label_ref, fail_label}, 0}
  end

  defp cmp_jump(:gt, :k, value, fail_label) do
    {:jmp, :jgt, :k, value, 0, {:label_ref, fail_label}}
  end

  defp cmp_jump(:gte, :k, value, fail_label) do
    {:jmp, :jge, :k, value, 0, {:label_ref, fail_label}}
  end

  defp cmp_jump(:lt, :k, value, fail_label) do
    {:jmp, :jge, :k, value, {:label_ref, fail_label}, 0}
  end

  defp cmp_jump(:lte, :k, value, fail_label) do
    {:jmp, :jgt, :k, value, {:label_ref, fail_label}, 0}
  end

  defp cmp_jump(:eq, :x, _, fail_label) do
    {:jmp, :jeq, :x, 0, {:label_ref, fail_label}}
  end

  defp cmp_jump(:neq, :x, _, fail_label) do
    {:jmp, :jeq, :x, {:label_ref, fail_label}, 0}
  end

  defp cmp_jump(:gt, :x, _, fail_label) do
    {:jmp, :jgt, :x, 0, {:label_ref, fail_label}}
  end

  defp cmp_jump(:gte, :x, _, fail_label) do
    {:jmp, :jge, :x, 0, {:label_ref, fail_label}}
  end

  defp cmp_jump(:lt, :x, _, fail_label) do
    {:jmp, :jge, :x, {:label_ref, fail_label}, 0}
  end

  defp cmp_jump(:lte, :x, _, fail_label) do
    {:jmp, :jgt, :x, {:label_ref, fail_label}, 0}
  end

  # Comparison jumps - succeed on true (for OR branches)
  defp cmp_jump_success(:eq, :k, value, success_label) do
    {:jmp, :jeq, :k, value, {:label_ref, success_label}, 0}
  end

  defp cmp_jump_success(:neq, :k, value, success_label) do
    {:jmp, :jeq, :k, value, 0, {:label_ref, success_label}}
  end

  defp cmp_jump_success(:gt, :k, value, success_label) do
    {:jmp, :jgt, :k, value, {:label_ref, success_label}, 0}
  end

  defp cmp_jump_success(:gte, :k, value, success_label) do
    {:jmp, :jge, :k, value, {:label_ref, success_label}, 0}
  end

  defp cmp_jump_success(:lt, :k, value, success_label) do
    {:jmp, :jge, :k, value, 0, {:label_ref, success_label}}
  end

  defp cmp_jump_success(:lte, :k, value, success_label) do
    {:jmp, :jgt, :k, value, 0, {:label_ref, success_label}}
  end

  defp cmp_jump_success(:eq, :x, _, success_label) do
    {:jmp, :jeq, :x, {:label_ref, success_label}, 0}
  end

  defp cmp_jump_success(:neq, :x, _, success_label) do
    {:jmp, :jeq, :x, 0, {:label_ref, success_label}}
  end

  defp cmp_jump_success(:gt, :x, _, success_label) do
    {:jmp, :jgt, :x, {:label_ref, success_label}, 0}
  end

  defp cmp_jump_success(:gte, :x, _, success_label) do
    {:jmp, :jge, :x, {:label_ref, success_label}, 0}
  end

  defp cmp_jump_success(:lt, :x, _, success_label) do
    {:jmp, :jge, :x, 0, {:label_ref, success_label}}
  end

  defp cmp_jump_success(:lte, :x, _, success_label) do
    {:jmp, :jgt, :x, 0, {:label_ref, success_label}}
  end
end
