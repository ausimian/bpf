defmodule BPF.Compiler do
  @moduledoc false

  alias BPF.IR.Pattern
  alias BPF.Program
  alias BPF.Compiler.{SSA, Liveness, RegAlloc, CodeGen}

  @reject_value 0

  @doc """
  Compile a list of IR clauses into a BPF program.

  ## Example

      iex> clauses = [%BPF.IR.Clause{...}]
      iex> BPF.Compiler.compile(clauses)
      {:ok, %BPF.Program{...}}
  """
  def compile(clauses) when is_list(clauses) do
    # Compile each clause through SSA pipeline
    instructions =
      clauses
      |> Enum.with_index()
      |> Enum.flat_map(fn {clause, idx} ->
        fail_label = :"clause_#{idx}_fail"

        ssa = SSA.from_clause(clause, fail_label)
        liveness = Liveness.analyze(ssa.ops)
        allocation = RegAlloc.allocate(ssa.ops, liveness)
        code = CodeGen.generate(ssa.ops, allocation)

        # Add clause label and fail label
        [{:label, :"clause_#{idx}"}] ++ code ++ [{:label, fail_label}]
      end)

    # Add final reject
    instructions = instructions ++ [{:label, :final_reject}, {:ret, :k, @reject_value}]

    # Resolve labels
    resolved = resolve_labels(instructions)

    bindings = collect_bindings(clauses)

    {:ok, Program.new(resolved, bindings)}
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
