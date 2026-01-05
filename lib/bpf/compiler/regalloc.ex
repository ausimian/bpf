defmodule BPF.Compiler.RegAlloc do
  @moduledoc false

  @max_scratch_slots 16

  @doc """
  Allocate registers for SSA operations.

  Returns a map from vreg to allocation:
  - `:a` - Keep in accumulator (ephemeral)
  - `{:mem, slot}` - Store in scratch memory slot
  """
  def allocate(ops, liveness) do
    # Sort vregs by definition order
    vregs =
      liveness
      |> Enum.sort_by(fn {_vreg, info} -> info.def end)
      |> Enum.map(fn {vreg, _} -> vreg end)

    # Track which slots are free at each point
    state = %{
      allocation: %{},
      # {vreg, slot, end_idx} sorted by end
      active: [],
      free_slots: Enum.to_list(0..(@max_scratch_slots - 1))
    }

    {allocation, _state} = allocate_vregs(vregs, ops, liveness, state)
    allocation
  end

  defp allocate_vregs([], _ops, _liveness, state) do
    {state.allocation, state}
  end

  defp allocate_vregs([vreg | rest], ops, liveness, state) do
    info = Map.get(liveness, vreg)
    def_idx = info.def
    last_use = info.last_use

    # Expire old intervals - free slots for vregs no longer live
    state = expire_old(state, def_idx)

    # Determine if this vreg needs a scratch slot
    allocation = determine_allocation(vreg, ops, liveness, state)

    state =
      case allocation do
        :a ->
          # No scratch slot needed
          %{state | allocation: Map.put(state.allocation, vreg, :a)}

        :need_slot ->
          # Allocate a scratch slot
          case state.free_slots do
            [slot | rest_slots] ->
              active = insert_active(state.active, {vreg, slot, last_use})

              %{
                state
                | allocation: Map.put(state.allocation, vreg, {:mem, slot}),
                  active: active,
                  free_slots: rest_slots
              }

            [] ->
              # No free slots - spill (shouldn't happen with <= 16 live values)
              # For now, raise an error
              raise "Out of scratch slots"
          end
      end

    allocate_vregs(rest, ops, liveness, state)
  end

  # Determine if a vreg can stay in A or needs a slot
  defp determine_allocation(vreg, ops, liveness, _state) do
    info = Map.get(liveness, vreg)

    cond do
      # Single use immediately after definition - can stay in A
      info.def + 1 == info.last_use and single_use?(info) ->
        :a

      # Value is only used once and we can keep it in A through intervening ops
      single_use?(info) and can_stay_in_a?(vreg, ops, liveness) ->
        :a

      # Otherwise needs a scratch slot
      true ->
        :need_slot
    end
  end

  defp single_use?(info), do: length(info.uses) == 1

  # Check if a value can stay in A from definition to use
  # This is true if no other operations that require A happen between def and use
  defp can_stay_in_a?(vreg, ops, liveness) do
    info = Map.get(liveness, vreg)

    # Get the ops between definition and use
    intervening = Enum.slice(ops, (info.def + 1)..(info.last_use - 1))

    # Check if any of them would clobber A (load/ALU) or use A for a different value
    not Enum.any?(intervening, fn op -> clobbers_a?(op, vreg) end)
  end

  # Check if an operation clobbers the A register
  defp clobbers_a?({:load_packet, _, _, _, _, _}, _vreg), do: true
  defp clobbers_a?({:load_imm, _, _}, _vreg), do: true
  defp clobbers_a?({:alu, _, _, _, _}, _vreg), do: true
  # Comparisons need to load their left operand to A
  defp clobbers_a?({:cmp, _, left, _, _}, vreg), do: left != vreg
  defp clobbers_a?({:cmp_success, _, left, _, _}, vreg), do: left != vreg
  defp clobbers_a?(_, _vreg), do: false

  # Expire active intervals that end before the current point
  defp expire_old(state, current_idx) do
    {expired, still_active} =
      Enum.split_while(state.active, fn {_vreg, _slot, end_idx} ->
        end_idx < current_idx
      end)

    # Free the slots from expired intervals
    freed_slots = Enum.map(expired, fn {_vreg, slot, _end} -> slot end)

    %{state | active: still_active, free_slots: Enum.sort(freed_slots ++ state.free_slots)}
  end

  # Insert into active list, maintaining sort by end index
  defp insert_active(active, entry = {_vreg, _slot, end_idx}) do
    {before, after_list} = Enum.split_while(active, fn {_, _, e} -> e <= end_idx end)
    before ++ [entry | after_list]
  end
end
