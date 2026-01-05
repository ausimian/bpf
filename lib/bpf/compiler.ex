defmodule BPF.Compiler do
  @moduledoc """
  Compiles IR clauses to BPF instructions.

  Takes `BPF.IR.Clause` structs and produces a list of BPF instruction tuples
  that can be executed by the interpreter or serialized to bytecode.
  """

  import Bitwise

  alias BPF.IR.{Pattern, Clause}
  alias BPF.Program

  @accept_value 0xFFFFFFFF
  @reject_value 0

  @doc """
  Compile a list of IR clauses into a BPF program.

  ## Example

      iex> clauses = [%BPF.IR.Clause{...}]
      iex> BPF.Compiler.compile(clauses)
      {:ok, %BPF.Program{...}}
  """
  def compile(clauses) when is_list(clauses) do
    # Generate instructions with symbolic labels
    {instructions, _state} = compile_clauses(clauses, initial_state())

    # Add final reject (fallthrough)
    instructions = instructions ++ [{:label, :final_reject}, {:ret, :k, @reject_value}]

    # Resolve labels to numeric offsets
    resolved = resolve_labels(instructions)

    bindings = collect_bindings(clauses)

    {:ok, Program.new(resolved, bindings)}
  end

  defp initial_state do
    %{
      scratch_next: 0,
      scratch_map: %{},
      label_counter: 0
    }
  end

  defp collect_bindings(clauses) do
    clauses
    |> Enum.flat_map(fn clause ->
      clause.pattern
      |> Pattern.bindings()
      |> Enum.map(fn {name, segment} -> {name, segment.offset} end)
    end)
    |> Map.new()
  end

  # Compile all clauses
  defp compile_clauses([], state) do
    {[], state}
  end

  defp compile_clauses([clause | rest], state) do
    {clause_label, state} = fresh_label(state, "clause")
    {next_clause_label, state} = fresh_label(state, "next_clause")

    {clause_instrs, state} = compile_clause(clause, next_clause_label, state)
    {rest_instrs, state} = compile_clauses(rest, state)

    instructions =
      [{:label, clause_label}] ++
        clause_instrs ++
        [{:label, next_clause_label}] ++
        rest_instrs

    {instructions, state}
  end

  # Compile a single clause
  defp compile_clause(%Clause{pattern: pattern, guard: guard, action: action}, fail_label, state) do
    # Compile literal checks
    {literal_instrs, state} = compile_literals(pattern, fail_label, state)

    # Load bindings needed for guard into scratch memory
    {binding_instrs, state} =
      if guard do
        compile_bindings(pattern, guard, state)
      else
        {[], state}
      end

    # Compile guard
    {guard_instrs, state} =
      if guard do
        compile_guard(guard, fail_label, state)
      else
        {[], state}
      end

    # Generate accept instruction
    accept_instr = compile_action(action)

    instructions = literal_instrs ++ binding_instrs ++ guard_instrs ++ [accept_instr]

    {instructions, state}
  end

  # Compile literal pattern checks
  defp compile_literals(pattern, fail_label, state) do
    literals = Pattern.literals(pattern)
    compile_literal_checks(literals, fail_label, state)
  end

  defp compile_literal_checks([], _fail_label, state), do: {[], state}

  defp compile_literal_checks([literal | rest], fail_label, state) do
    {instrs, state} = compile_literal_check(literal, fail_label, state)
    {rest_instrs, state} = compile_literal_checks(rest, fail_label, state)
    {instrs ++ rest_instrs, state}
  end

  defp compile_literal_check(segment, fail_label, state) do
    offset_bits = segment.offset
    size_bits = segment.size * segment.unit
    value = segment.value

    # Handle sub-byte and byte-aligned loads
    cond do
      # Byte-aligned, standard size
      rem(offset_bits, 8) == 0 and size_bits in [8, 16, 32] ->
        byte_offset = div(offset_bits, 8)
        size_atom = size_to_atom(size_bits)

        instrs = [
          {:ld, size_atom, [:k, byte_offset]},
          {:jmp, :jeq, :k, value, 0, {:label_ref, fail_label}}
        ]

        {instrs, state}

      # Sub-byte field - need to load byte and mask/shift
      size_bits < 8 ->
        compile_subbyte_literal(offset_bits, size_bits, value, fail_label, state)

      # Multi-byte but not standard size or not aligned
      true ->
        compile_complex_literal(offset_bits, size_bits, value, fail_label, state)
    end
  end

  defp compile_subbyte_literal(offset_bits, size_bits, value, fail_label, state) do
    byte_offset = div(offset_bits, 8)
    bit_offset_in_byte = rem(offset_bits, 8)

    # Calculate mask and shift
    # For big-endian: upper bits come first
    # If bit_offset is 0 and size is 4, we want upper 4 bits: shift right by 4
    # If bit_offset is 4 and size is 4, we want lower 4 bits: mask with 0x0F
    shift_amount = 8 - bit_offset_in_byte - size_bits
    mask = (1 <<< size_bits) - 1

    instrs =
      if shift_amount > 0 do
        [
          {:ld, :b, [:k, byte_offset]},
          {:rsh, shift_amount},
          {:and, mask},
          {:jmp, :jeq, :k, value, 0, {:label_ref, fail_label}}
        ]
      else
        [
          {:ld, :b, [:k, byte_offset]},
          {:and, mask},
          {:jmp, :jeq, :k, value, 0, {:label_ref, fail_label}}
        ]
      end

    {instrs, state}
  end

  defp compile_complex_literal(_offset_bits, _size_bits, _value, _fail_label, state) do
    # TODO: Handle complex cases (multi-byte non-aligned)
    {[], state}
  end

  # Load bindings needed for guard evaluation into scratch memory
  defp compile_bindings(pattern, guard, state) do
    needed_bindings = find_guard_bindings(guard)
    bindings = Pattern.bindings(pattern)

    {instrs, state} =
      Enum.reduce(needed_bindings, {[], state}, fn name, {acc, st} ->
        case Map.get(bindings, name) do
          nil ->
            {acc, st}

          segment ->
            {load_instrs, st} = compile_binding_load(name, segment, st)
            {acc ++ load_instrs, st}
        end
      end)

    {instrs, state}
  end

  defp find_guard_bindings(guard) do
    find_bindings_in_expr(guard, MapSet.new()) |> MapSet.to_list()
  end

  defp find_bindings_in_expr({:binding, name}, acc), do: MapSet.put(acc, name)
  defp find_bindings_in_expr({:literal, _}, acc), do: acc

  defp find_bindings_in_expr({:compare, _, left, right}, acc) do
    acc = find_bindings_in_expr(left, acc)
    find_bindings_in_expr(right, acc)
  end

  defp find_bindings_in_expr({:logical, _, left, right}, acc) do
    acc = find_bindings_in_expr(left, acc)
    if right, do: find_bindings_in_expr(right, acc), else: acc
  end

  defp find_bindings_in_expr({:bitwise, _, left, right}, acc) do
    acc = find_bindings_in_expr(left, acc)
    if right, do: find_bindings_in_expr(right, acc), else: acc
  end

  defp find_bindings_in_expr({:arith, _, left, right}, acc) do
    acc = find_bindings_in_expr(left, acc)
    find_bindings_in_expr(right, acc)
  end

  defp compile_binding_load(name, segment, state) do
    offset_bits = segment.offset
    size_bits = segment.size * segment.unit

    # Allocate scratch slot
    {slot, state} = allocate_scratch(state, name)

    cond do
      # Byte-aligned standard size
      rem(offset_bits, 8) == 0 and size_bits in [8, 16, 32] ->
        byte_offset = div(offset_bits, 8)
        size_atom = size_to_atom(size_bits)

        instrs = [
          {:ld, size_atom, [:k, byte_offset]},
          {:st, slot}
        ]

        {instrs, state}

      # Sub-byte field
      size_bits < 8 ->
        compile_subbyte_binding_load(offset_bits, size_bits, slot, state)

      # Other cases
      true ->
        {[], state}
    end
  end

  defp compile_subbyte_binding_load(offset_bits, size_bits, slot, state) do
    byte_offset = div(offset_bits, 8)
    bit_offset_in_byte = rem(offset_bits, 8)

    shift_amount = 8 - bit_offset_in_byte - size_bits
    mask = (1 <<< size_bits) - 1

    instrs =
      if shift_amount > 0 do
        [
          {:ld, :b, [:k, byte_offset]},
          {:rsh, shift_amount},
          {:and, mask},
          {:st, slot}
        ]
      else
        [
          {:ld, :b, [:k, byte_offset]},
          {:and, mask},
          {:st, slot}
        ]
      end

    {instrs, state}
  end

  defp allocate_scratch(state, name) do
    case Map.get(state.scratch_map, name) do
      nil ->
        slot = state.scratch_next

        state = %{
          state
          | scratch_next: slot + 1,
            scratch_map: Map.put(state.scratch_map, name, slot)
        }

        {slot, state}

      slot ->
        {slot, state}
    end
  end

  defp get_scratch(state, name) do
    Map.fetch!(state.scratch_map, name)
  end

  # Compile guard expression
  defp compile_guard(guard, fail_label, state) do
    compile_guard_expr(guard, fail_label, state)
  end

  # Comparison: load operands, compare, jump
  defp compile_guard_expr({:compare, op, left, right}, fail_label, state) do
    {left_instrs, state} = load_operand_to_a(left, state)
    {right_instrs, cmp_source, state} = prepare_comparison_right(right, state)

    jump_instr = compile_comparison_jump(op, cmp_source, fail_label)

    {left_instrs ++ right_instrs ++ [jump_instr], state}
  end

  # Logical AND: short-circuit - if left fails, skip right
  defp compile_guard_expr({:logical, :and, left, right}, fail_label, state) do
    {left_instrs, state} = compile_guard_expr(left, fail_label, state)
    {right_instrs, state} = compile_guard_expr(right, fail_label, state)
    {left_instrs ++ right_instrs, state}
  end

  # Logical OR: short-circuit - if left succeeds, skip right
  defp compile_guard_expr({:logical, :or, left, right}, fail_label, state) do
    {success_label, state} = fresh_label(state, "or_success")

    # Compile left with success_label as the "true" target
    {left_instrs, state} = compile_guard_expr_or_branch(left, success_label, fail_label, state)
    {right_instrs, state} = compile_guard_expr(right, fail_label, state)

    # If left succeeded, we jump to success_label which skips right branch
    {left_instrs ++ right_instrs ++ [{:label, success_label}], state}
  end

  # For OR branches, we need inverted logic
  defp compile_guard_expr_or_branch({:compare, op, left, right}, success_label, _fail_label, state) do
    {left_instrs, state} = load_operand_to_a(left, state)
    {right_instrs, cmp_source, state} = prepare_comparison_right(right, state)

    # For OR, if condition is true, jump to success (skip remaining)
    jump_instr = compile_comparison_jump_inverted(op, cmp_source, success_label)

    {left_instrs ++ right_instrs ++ [jump_instr], state}
  end

  # Handle nested OR within OR branch - propagate the success label
  defp compile_guard_expr_or_branch({:logical, :or, left, right}, success_label, fail_label, state) do
    # For nested OR: if left succeeds, jump to success; else try right
    {left_instrs, state} = compile_guard_expr_or_branch(left, success_label, fail_label, state)
    {right_instrs, state} = compile_guard_expr_or_branch(right, success_label, fail_label, state)
    {left_instrs ++ right_instrs, state}
  end

  # Handle AND within OR branch - the AND must fully succeed to jump to success
  defp compile_guard_expr_or_branch({:logical, :and, left, right}, success_label, _fail_label, state) do
    # For AND within OR: both must succeed. If left fails, try next OR branch.
    # Create a local fail label for this AND
    {and_fail_label, state} = fresh_label(state, "and_fail")

    {left_instrs, state} = compile_guard_expr(left, and_fail_label, state)
    {right_instrs, state} = compile_guard_expr(right, and_fail_label, state)

    # If we get here, AND succeeded - jump to success
    instrs =
      left_instrs ++
        right_instrs ++
        [{:jmp, :ja, {:label_ref, success_label}}, {:label, and_fail_label}]

    {instrs, state}
  end

  defp compile_guard_expr_or_branch(other, _success_label, fail_label, state) do
    compile_guard_expr(other, fail_label, state)
  end

  # Load an operand into the accumulator
  defp load_operand_to_a({:binding, name}, state) do
    slot = get_scratch(state, name)
    {[{:ld, :mem, slot}], state}
  end

  defp load_operand_to_a({:literal, value}, state) do
    {[{:ld, :imm, value}], state}
  end

  defp load_operand_to_a({:arith, op, left, right}, state) do
    # Load left into A
    {left_instrs, state} = load_operand_to_a(left, state)

    # Handle right operand
    case right do
      {:literal, value} ->
        alu_instr = arith_to_alu(op, value)
        {left_instrs ++ [alu_instr], state}

      {:binding, name} ->
        slot = get_scratch(state, name)
        # Load right into X, then operate
        {left_instrs ++ [{:st, 15}, {:ld, :mem, slot}, :tax, {:ld, :mem, 15}, arith_to_alu_x(op)],
         state}

      _ ->
        # Complex right operand - evaluate, store, then operate
        {right_instrs, state} = load_operand_to_a(right, state)

        instrs =
          left_instrs ++
            [{:st, 15}] ++ right_instrs ++ [:tax, {:ld, :mem, 15}, arith_to_alu_x(op)]

        {instrs, state}
    end
  end

  defp load_operand_to_a({:bitwise, op, left, right}, state) do
    {left_instrs, state} = load_operand_to_a(left, state)

    case right do
      {:literal, value} ->
        alu_instr = bitwise_to_alu(op, value)
        {left_instrs ++ [alu_instr], state}

      {:binding, name} ->
        slot = get_scratch(state, name)
        {left_instrs ++ [{:st, 15}, {:ld, :mem, slot}, :tax, {:ld, :mem, 15}, bitwise_to_alu_x(op)],
         state}

      nil ->
        # Unary op (bnot) - bitwise NOT is XOR with all 1s
        {left_instrs ++ [{:xor, 0xFFFFFFFF}], state}

      _ ->
        {right_instrs, state} = load_operand_to_a(right, state)

        instrs =
          left_instrs ++
            [{:st, 15}] ++ right_instrs ++ [:tax, {:ld, :mem, 15}, bitwise_to_alu_x(op)]

        {instrs, state}
    end
  end

  defp prepare_comparison_right({:literal, value}, state) do
    {[], {:k, value}, state}
  end

  defp prepare_comparison_right({:binding, name}, state) do
    slot = get_scratch(state, name)
    # Save A to slot 14 (reserved for comparison), load binding into X, restore A
    {[{:st, 14}, {:ld, :mem, slot}, :tax, {:ld, :mem, 14}], :x, state}
  end

  defp prepare_comparison_right(expr, state) do
    # Save A to slot 14 (reserved for comparison), evaluate complex expression,
    # put result in X, restore A from slot 14.
    # We use slot 14 because load_operand_to_a uses slot 15 for intermediate operations.
    {instrs, state} = load_operand_to_a(expr, state)
    {[{:st, 14}] ++ instrs ++ [:tax, {:ld, :mem, 14}], :x, state}
  end

  defp compile_comparison_jump(op, {:k, value}, fail_label) do
    case op do
      :eq -> {:jmp, :jeq, :k, value, 0, {:label_ref, fail_label}}
      :neq -> {:jmp, :jeq, :k, value, {:label_ref, fail_label}, 0}
      :gt -> {:jmp, :jgt, :k, value, 0, {:label_ref, fail_label}}
      :gte -> {:jmp, :jge, :k, value, 0, {:label_ref, fail_label}}
      # A < k is NOT (A >= k)
      :lt -> {:jmp, :jge, :k, value, {:label_ref, fail_label}, 0}
      # A <= k is NOT (A > k)
      :lte -> {:jmp, :jgt, :k, value, {:label_ref, fail_label}, 0}
    end
  end

  defp compile_comparison_jump(op, :x, fail_label) do
    case op do
      :eq -> {:jmp, :jeq, :x, 0, {:label_ref, fail_label}}
      :neq -> {:jmp, :jeq, :x, {:label_ref, fail_label}, 0}
      :gt -> {:jmp, :jgt, :x, 0, {:label_ref, fail_label}}
      :gte -> {:jmp, :jge, :x, 0, {:label_ref, fail_label}}
      :lt -> {:jmp, :jge, :x, {:label_ref, fail_label}, 0}
      :lte -> {:jmp, :jgt, :x, {:label_ref, fail_label}, 0}
    end
  end

  # Inverted jumps for OR branches (jump on success)
  defp compile_comparison_jump_inverted(op, {:k, value}, success_label) do
    case op do
      :eq -> {:jmp, :jeq, :k, value, {:label_ref, success_label}, 0}
      :neq -> {:jmp, :jeq, :k, value, 0, {:label_ref, success_label}}
      :gt -> {:jmp, :jgt, :k, value, {:label_ref, success_label}, 0}
      :gte -> {:jmp, :jge, :k, value, {:label_ref, success_label}, 0}
      :lt -> {:jmp, :jge, :k, value, 0, {:label_ref, success_label}}
      :lte -> {:jmp, :jgt, :k, value, 0, {:label_ref, success_label}}
    end
  end

  defp compile_comparison_jump_inverted(op, :x, success_label) do
    case op do
      :eq -> {:jmp, :jeq, :x, {:label_ref, success_label}, 0}
      :neq -> {:jmp, :jeq, :x, 0, {:label_ref, success_label}}
      :gt -> {:jmp, :jgt, :x, {:label_ref, success_label}, 0}
      :gte -> {:jmp, :jge, :x, {:label_ref, success_label}, 0}
      :lt -> {:jmp, :jge, :x, 0, {:label_ref, success_label}}
      :lte -> {:jmp, :jgt, :x, 0, {:label_ref, success_label}}
    end
  end

  defp arith_to_alu(:add, k), do: {:add, k}
  defp arith_to_alu(:sub, k), do: {:sub, k}
  defp arith_to_alu(:mul, k), do: {:mul, k}
  defp arith_to_alu(:div, k), do: {:div, k}

  defp arith_to_alu_x(:add), do: {:add, :x}
  defp arith_to_alu_x(:sub), do: {:sub, :x}
  defp arith_to_alu_x(:mul), do: {:mul, :x}
  defp arith_to_alu_x(:div), do: {:div, :x}

  defp bitwise_to_alu(:band, k), do: {:and, k}
  defp bitwise_to_alu(:bor, k), do: {:or, k}
  defp bitwise_to_alu(:bxor, k), do: {:xor, k}

  defp bitwise_to_alu_x(:band), do: {:and, :x}
  defp bitwise_to_alu_x(:bor), do: {:or, :x}
  defp bitwise_to_alu_x(:bxor), do: {:xor, :x}

  defp compile_action(:accept), do: {:ret, :k, @accept_value}
  defp compile_action(:reject), do: {:ret, :k, @reject_value}
  defp compile_action({:accept, n}), do: {:ret, :k, n}

  defp fresh_label(state, prefix) do
    label = :"#{prefix}_#{state.label_counter}"
    {label, %{state | label_counter: state.label_counter + 1}}
  end

  defp size_to_atom(8), do: :b
  defp size_to_atom(16), do: :h
  defp size_to_atom(32), do: :w

  # Label resolution - convert symbolic labels to numeric offsets
  defp resolve_labels(instructions) do
    # First pass: build label -> index map (excluding labels themselves)
    {label_map, _} =
      Enum.reduce(instructions, {%{}, 0}, fn
        {:label, name}, {map, idx} ->
          {Map.put(map, name, idx), idx}

        _instr, {map, idx} ->
          {map, idx + 1}
      end)

    # Second pass: remove labels, resolve references
    instructions
    |> Enum.reject(&match?({:label, _}, &1))
    |> Enum.with_index()
    |> Enum.map(fn {instr, idx} -> resolve_instruction(instr, idx, label_map) end)
  end

  defp resolve_instruction({:jmp, :jeq, :k, k, jt, jf}, idx, labels) do
    {:jmp, :jeq, :k, k, resolve_offset(jt, idx, labels), resolve_offset(jf, idx, labels)}
  end

  defp resolve_instruction({:jmp, :jgt, :k, k, jt, jf}, idx, labels) do
    {:jmp, :jgt, :k, k, resolve_offset(jt, idx, labels), resolve_offset(jf, idx, labels)}
  end

  defp resolve_instruction({:jmp, :jge, :k, k, jt, jf}, idx, labels) do
    {:jmp, :jge, :k, k, resolve_offset(jt, idx, labels), resolve_offset(jf, idx, labels)}
  end

  defp resolve_instruction({:jmp, :jset, :k, k, jt, jf}, idx, labels) do
    {:jmp, :jset, :k, k, resolve_offset(jt, idx, labels), resolve_offset(jf, idx, labels)}
  end

  defp resolve_instruction({:jmp, op, :x, jt, jf}, idx, labels) do
    {:jmp, op, :x, resolve_offset(jt, idx, labels), resolve_offset(jf, idx, labels)}
  end

  defp resolve_instruction({:jmp, :ja, target}, idx, labels) do
    {:jmp, :ja, resolve_offset(target, idx, labels)}
  end

  defp resolve_instruction(instr, _idx, _labels), do: instr

  defp resolve_offset({:label_ref, label}, current_idx, labels) do
    target_idx = Map.fetch!(labels, label)
    # Offset is relative to NEXT instruction
    target_idx - current_idx - 1
  end

  defp resolve_offset(n, _idx, _labels) when is_integer(n), do: n
end
