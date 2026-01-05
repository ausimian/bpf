defmodule BPF.Compiler.SSA do
  @moduledoc false

  alias BPF.IR.Clause

  import Bitwise

  defstruct [
    # List of SSA operations
    :ops,
    # Next virtual register ID
    :next_vreg,
    # Map of binding name to vreg
    :bindings,
    # Label to jump on clause failure
    :fail_label
  ]

  # SSA Operations:
  # {:load_packet, vreg, size, byte_offset, bit_shift, mask}  - Load from packet with optional shift/mask
  # {:load_imm, vreg, value}                                  - Load immediate value
  # {:alu, vreg, op, src1, src2}                              - ALU operation (src2 can be vreg or {:imm, n})
  # {:cmp, op, left, right, fail_label}                       - Compare and jump to fail_label if false
  # {:jump, label}                                            - Unconditional jump
  # {:label, name}                                            - Jump target
  # {:ret, :accept | :reject | {:imm, n} | vreg}              - Return

  @doc """
  Convert an IR clause to SSA form.
  """
  def from_clause(%Clause{pattern: pattern, guard: guard, action: action}, fail_label) do
    state = %__MODULE__{
      ops: [],
      next_vreg: 0,
      bindings: %{},
      fail_label: fail_label
    }

    # Phase 1: Determine which bindings are actually used
    used_bindings = find_used_bindings(guard, action)

    # Phase 2: Compile pattern (literals and used bindings)
    state = compile_pattern(pattern, used_bindings, state)

    # Phase 3: Compile guard
    state = if guard, do: compile_guard(guard, state), else: state

    # Phase 4: Compile action
    state = compile_action(action, state)

    %{state | ops: Enum.reverse(state.ops)}
  end

  @doc """
  Get the list of SSA operations.
  """
  def operations(%__MODULE__{ops: ops}), do: ops

  # Find all bindings referenced in guard and action
  defp find_used_bindings(guard, action) do
    guard_bindings = if guard, do: find_bindings_in_expr(guard), else: MapSet.new()
    action_bindings = find_bindings_in_action(action)
    MapSet.union(guard_bindings, action_bindings)
  end

  defp find_bindings_in_expr({:binding, name}), do: MapSet.new([name])
  defp find_bindings_in_expr({:literal, _}), do: MapSet.new()

  defp find_bindings_in_expr({:compare, _, left, right}) do
    MapSet.union(find_bindings_in_expr(left), find_bindings_in_expr(right))
  end

  defp find_bindings_in_expr({:logical, _, left, right}) do
    left_bindings = find_bindings_in_expr(left)
    right_bindings = if right, do: find_bindings_in_expr(right), else: MapSet.new()
    MapSet.union(left_bindings, right_bindings)
  end

  defp find_bindings_in_expr({:arith, _, left, right}) do
    MapSet.union(find_bindings_in_expr(left), find_bindings_in_expr(right))
  end

  defp find_bindings_in_expr({:bitwise, _, left, right}) do
    left_bindings = find_bindings_in_expr(left)
    right_bindings = if right, do: find_bindings_in_expr(right), else: MapSet.new()
    MapSet.union(left_bindings, right_bindings)
  end

  defp find_bindings_in_action({:return_expr, expr}), do: find_bindings_in_expr(expr)
  defp find_bindings_in_action({:return_cond, guard}), do: find_bindings_in_expr(guard)
  defp find_bindings_in_action(_), do: MapSet.new()

  # Compile pattern: literals as checks, bindings as loads
  defp compile_pattern(pattern, used_bindings, state) do
    segments = pattern.segments

    # First compile literal checks
    state = compile_literals(segments, state)

    # Then compile binding loads (only for used bindings)
    compile_bindings(segments, used_bindings, state)
  end

  defp compile_literals(segments, state) do
    segments
    |> Enum.filter(fn seg -> seg.type == :literal end)
    |> Enum.reduce(state, fn seg, st -> compile_literal_check(seg, st) end)
  end

  defp compile_literal_check(segment, state) do
    offset_bits = segment.offset
    size_bits = segment.size * segment.unit
    value = segment.value

    cond do
      # Byte-aligned standard size
      rem(offset_bits, 8) == 0 and size_bits in [8, 16, 32] ->
        byte_offset = div(offset_bits, 8)
        size = bits_to_size(size_bits)

        # Load, compare, jump on failure
        {vreg, state} = fresh_vreg(state)
        state = emit(state, {:load_packet, vreg, size, byte_offset, 0, nil})
        emit(state, {:cmp, :eq, vreg, {:imm, value}, state.fail_label})

      # Sub-byte field
      size_bits < 8 ->
        compile_subbyte_literal_check(offset_bits, size_bits, value, state)

      # Multi-byte non-standard (not implemented yet)
      true ->
        state
    end
  end

  defp compile_subbyte_literal_check(offset_bits, size_bits, value, state) do
    byte_offset = div(offset_bits, 8)
    bit_offset_in_byte = rem(offset_bits, 8)

    shift_amount = 8 - bit_offset_in_byte - size_bits
    mask = (1 <<< size_bits) - 1

    {vreg, state} = fresh_vreg(state)
    state = emit(state, {:load_packet, vreg, :byte, byte_offset, shift_amount, mask})
    emit(state, {:cmp, :eq, vreg, {:imm, value}, state.fail_label})
  end

  defp compile_bindings(segments, used_bindings, state) do
    segments
    |> Enum.filter(fn seg ->
      seg.type == :binding and MapSet.member?(used_bindings, seg.name)
    end)
    |> Enum.reduce(state, fn seg, st -> compile_binding_load(seg, st) end)
  end

  defp compile_binding_load(segment, state) do
    offset_bits = segment.offset
    size_bits = segment.size * segment.unit
    name = segment.name

    cond do
      # Byte-aligned standard size
      rem(offset_bits, 8) == 0 and size_bits in [8, 16, 32] ->
        byte_offset = div(offset_bits, 8)
        size = bits_to_size(size_bits)

        {vreg, state} = fresh_vreg(state)
        state = emit(state, {:load_packet, vreg, size, byte_offset, 0, nil})
        register_binding(state, name, vreg)

      # Sub-byte field
      size_bits < 8 ->
        compile_subbyte_binding_load(name, offset_bits, size_bits, state)

      # Multi-byte non-standard
      true ->
        state
    end
  end

  defp compile_subbyte_binding_load(name, offset_bits, size_bits, state) do
    byte_offset = div(offset_bits, 8)
    bit_offset_in_byte = rem(offset_bits, 8)

    shift_amount = 8 - bit_offset_in_byte - size_bits
    mask = (1 <<< size_bits) - 1

    {vreg, state} = fresh_vreg(state)
    state = emit(state, {:load_packet, vreg, :byte, byte_offset, shift_amount, mask})
    register_binding(state, name, vreg)
  end

  # Compile guard expression
  defp compile_guard({:compare, op, left, right}, state) do
    {left_vreg, state} = compile_operand(left, state)
    {right_src, state} = compile_operand_src(right, state)
    emit(state, {:cmp, op, left_vreg, right_src, state.fail_label})
  end

  defp compile_guard({:logical, :and, left, right}, state) do
    # Short-circuit AND: evaluate left, then right
    state = compile_guard(left, state)
    compile_guard(right, state)
  end

  defp compile_guard({:logical, :or, left, right}, state) do
    # Short-circuit OR: if left succeeds, skip right
    {success_label, state} = fresh_label(state, "or_success")

    # For OR, we need inverted logic on left branch
    state = compile_guard_or_branch(left, success_label, state)
    state = compile_guard(right, state)
    emit(state, {:label, success_label})
  end

  # For OR branch, jump to success if condition is true
  defp compile_guard_or_branch({:compare, op, left, right}, success_label, state) do
    {left_vreg, state} = compile_operand(left, state)
    {right_src, state} = compile_operand_src(right, state)
    # Use inverted comparison - jump to success on match
    emit(state, {:cmp_success, op, left_vreg, right_src, success_label})
  end

  defp compile_guard_or_branch({:logical, :or, left, right}, success_label, state) do
    state = compile_guard_or_branch(left, success_label, state)
    compile_guard_or_branch(right, success_label, state)
  end

  defp compile_guard_or_branch({:logical, :and, left, right}, success_label, state) do
    {and_fail_label, state} = fresh_label(state, "and_fail")

    # Both must succeed for AND
    old_fail = state.fail_label
    state = %{state | fail_label: and_fail_label}
    state = compile_guard(left, state)
    state = compile_guard(right, state)
    state = %{state | fail_label: old_fail}

    # If we get here, AND succeeded
    state = emit(state, {:jump, success_label})
    emit(state, {:label, and_fail_label})
  end

  defp compile_guard_or_branch(other, _success_label, state) do
    compile_guard(other, state)
  end

  # Compile an operand to a vreg, returning the vreg
  defp compile_operand({:binding, name}, state) do
    vreg = Map.fetch!(state.bindings, name)
    {vreg, state}
  end

  defp compile_operand({:literal, value}, state) do
    {vreg, state} = fresh_vreg(state)
    state = emit(state, {:load_imm, vreg, value})
    {vreg, state}
  end

  defp compile_operand({:arith, op, left, right}, state) do
    {left_vreg, state} = compile_operand(left, state)
    {right_src, state} = compile_operand_src(right, state)
    {result_vreg, state} = fresh_vreg(state)
    state = emit(state, {:alu, result_vreg, op, left_vreg, right_src})
    {result_vreg, state}
  end

  defp compile_operand({:bitwise, op, left, right}, state) do
    {left_vreg, state} = compile_operand(left, state)

    case right do
      nil ->
        # Unary (bnot)
        {result_vreg, state} = fresh_vreg(state)
        state = emit(state, {:alu, result_vreg, :bnot, left_vreg, nil})
        {result_vreg, state}

      _ ->
        {right_src, state} = compile_operand_src(right, state)
        {result_vreg, state} = fresh_vreg(state)
        state = emit(state, {:alu, result_vreg, op, left_vreg, right_src})
        {result_vreg, state}
    end
  end

  # Compile operand to either {:imm, value} or vreg (for RHS of operations)
  defp compile_operand_src({:literal, value}, state) do
    {{:imm, value}, state}
  end

  defp compile_operand_src(other, state) do
    compile_operand(other, state)
  end

  # Compile action
  defp compile_action(:accept, state) do
    emit(state, {:ret, :accept})
  end

  defp compile_action(:reject, state) do
    emit(state, {:ret, :reject})
  end

  defp compile_action({:accept, n}, state) do
    emit(state, {:ret, {:imm, n}})
  end

  defp compile_action({:return_expr, expr}, state) do
    {vreg, state} = compile_operand(expr, state)
    emit(state, {:ret, vreg})
  end

  defp compile_action({:return_cond, guard}, state) do
    {reject_label, state} = fresh_label(state, "cond_reject")
    old_fail = state.fail_label
    state = %{state | fail_label: reject_label}
    state = compile_guard(guard, state)
    state = %{state | fail_label: old_fail}
    state = emit(state, {:ret, :accept})
    state = emit(state, {:label, reject_label})
    emit(state, {:ret, :reject})
  end

  # Helper functions
  defp fresh_vreg(state) do
    vreg = state.next_vreg
    {vreg, %{state | next_vreg: vreg + 1}}
  end

  defp fresh_label(state, prefix) do
    # Use vreg counter for unique labels
    {id, state} = fresh_vreg(state)
    {:"#{prefix}_#{id}", state}
  end

  defp emit(state, op) do
    %{state | ops: [op | state.ops]}
  end

  defp register_binding(state, name, vreg) do
    %{state | bindings: Map.put(state.bindings, name, vreg)}
  end

  defp bits_to_size(8), do: :byte
  defp bits_to_size(16), do: :half
  defp bits_to_size(32), do: :word
end
