defmodule BPF.Compiler.Liveness do
  @moduledoc false

  @doc """
  Analyze liveness of SSA operations.

  Returns a map from vreg to %{def: index, last_use: index, uses: [index]}
  where index is the instruction position.
  """
  def analyze(ops) do
    ops
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {op, idx}, acc ->
      acc
      |> record_def(op, idx)
      |> record_uses(op, idx)
    end)
  end

  @doc """
  Get the live range for a vreg as {start, end} tuple.
  """
  def live_range(analysis, vreg) do
    case Map.get(analysis, vreg) do
      nil -> nil
      %{def: def_idx, last_use: last_use} -> {def_idx, last_use}
    end
  end

  @doc """
  Check if two vregs have overlapping live ranges.
  """
  def overlaps?(analysis, vreg1, vreg2) do
    case {live_range(analysis, vreg1), live_range(analysis, vreg2)} do
      {nil, _} ->
        false

      {_, nil} ->
        false

      {{s1, e1}, {s2, e2}} ->
        # Ranges overlap if one starts before the other ends
        s1 <= e2 and s2 <= e1
    end
  end

  @doc """
  Get all vregs live at a specific instruction index.
  """
  def live_at(analysis, idx) do
    analysis
    |> Enum.filter(fn {_vreg, %{def: def_idx, last_use: last_use}} ->
      def_idx <= idx and idx <= last_use
    end)
    |> Enum.map(fn {vreg, _} -> vreg end)
  end

  # Record definition site
  defp record_def(acc, {:load_packet, vreg, _size, _offset, _shift, _mask}, idx) do
    update_vreg(acc, vreg, :def, idx)
  end

  defp record_def(acc, {:load_imm, vreg, _value}, idx) do
    update_vreg(acc, vreg, :def, idx)
  end

  defp record_def(acc, {:load_len, vreg}, idx) do
    update_vreg(acc, vreg, :def, idx)
  end

  defp record_def(acc, {:alu, vreg, _op, _src1, _src2}, idx) do
    update_vreg(acc, vreg, :def, idx)
  end

  defp record_def(acc, _op, _idx), do: acc

  # Record use sites
  defp record_uses(acc, {:cmp, _op, left, right, _label}, idx) do
    acc
    |> maybe_record_use(left, idx)
    |> maybe_record_use(right, idx)
  end

  defp record_uses(acc, {:cmp_success, _op, left, right, _label}, idx) do
    acc
    |> maybe_record_use(left, idx)
    |> maybe_record_use(right, idx)
  end

  defp record_uses(acc, {:alu, _vreg, _op, src1, src2}, idx) do
    acc
    |> maybe_record_use(src1, idx)
    |> maybe_record_use(src2, idx)
  end

  defp record_uses(acc, {:ret, src}, idx) do
    maybe_record_use(acc, src, idx)
  end

  defp record_uses(acc, _op, _idx), do: acc

  # Only record uses for vregs (not immediates or atoms)
  defp maybe_record_use(acc, vreg, idx) when is_integer(vreg) do
    update_vreg(acc, vreg, :use, idx)
  end

  defp maybe_record_use(acc, {:imm, _}, _idx), do: acc
  defp maybe_record_use(acc, :accept, _idx), do: acc
  defp maybe_record_use(acc, :reject, _idx), do: acc
  defp maybe_record_use(acc, nil, _idx), do: acc
  defp maybe_record_use(acc, _, _idx), do: acc

  # Update vreg info
  defp update_vreg(acc, vreg, :def, idx) do
    Map.update(acc, vreg, %{def: idx, last_use: idx, uses: []}, fn info ->
      %{info | def: idx}
    end)
  end

  defp update_vreg(acc, vreg, :use, idx) do
    Map.update(acc, vreg, %{def: 0, last_use: idx, uses: [idx]}, fn info ->
      %{info | last_use: max(info.last_use, idx), uses: [idx | info.uses]}
    end)
  end
end
