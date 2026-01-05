defmodule BPF.Interpreter do
  @moduledoc false

  import Bitwise

  defstruct [:a, :x, :pc, :mem, :packet]

  @type t :: %__MODULE__{
          a: non_neg_integer(),
          x: non_neg_integer(),
          pc: non_neg_integer(),
          mem: %{non_neg_integer() => non_neg_integer()},
          packet: binary()
        }

  @doc """
  Run a BPF program on a packet.

  Returns `{:ok, return_value}` on success, or `{:error, reason}` on failure.
  """
  def run(%BPF.Program{instructions: instructions}, packet) when is_binary(packet) do
    state = %__MODULE__{
      a: 0,
      x: 0,
      pc: 0,
      mem: %{},
      packet: packet
    }

    execute(instructions, state)
  end

  def run(instructions, packet) when is_list(instructions) and is_binary(packet) do
    run(%BPF.Program{instructions: instructions}, packet)
  end

  defp execute(instructions, %{pc: pc} = state) do
    case Enum.at(instructions, pc) do
      nil ->
        {:error, :fell_off_end}

      instruction ->
        case step(instruction, state) do
          {:continue, new_state} ->
            execute(instructions, new_state)

          {:return, value} ->
            {:ok, value}

          {:error, _} = error ->
            error
        end
    end
  end

  # Load instructions - A = packet[k] (absolute)
  defp step({:ld, :w, [:k, k]}, state), do: load_packet(state, k, 4)
  defp step({:ld, :h, [:k, k]}, state), do: load_packet(state, k, 2)
  defp step({:ld, :b, [:k, k]}, state), do: load_packet(state, k, 1)

  # Load instructions - A = packet[X + k] (indirect)
  defp step({:ld, :w, [:x, k]}, %{x: x} = state), do: load_packet(state, x + k, 4)
  defp step({:ld, :h, [:x, k]}, %{x: x} = state), do: load_packet(state, x + k, 2)
  defp step({:ld, :b, [:x, k]}, %{x: x} = state), do: load_packet(state, x + k, 1)

  # Load immediate - A = k
  defp step({:ld, :imm, k}, state), do: {:continue, advance(%{state | a: k})}

  # Load packet length - A = len
  defp step({:ld, :len}, %{packet: packet} = state) do
    {:continue, advance(%{state | a: byte_size(packet)})}
  end

  # Load from scratch memory - A = M[k]
  defp step({:ld, :mem, k}, %{mem: mem} = state) do
    {:continue, advance(%{state | a: Map.get(mem, k, 0)})}
  end

  # Load into X - packet[k] (absolute)
  defp step({:ldx, :w, [:k, k]}, state), do: load_packet_x(state, k, 4)
  defp step({:ldx, :h, [:k, k]}, state), do: load_packet_x(state, k, 2)
  defp step({:ldx, :b, [:k, k]}, state), do: load_packet_x(state, k, 1)

  # Load immediate into X - X = k
  defp step({:ldx, :imm, k}, state), do: {:continue, advance(%{state | x: k})}

  # Load packet length into X - X = len
  defp step({:ldx, :len}, %{packet: packet} = state) do
    {:continue, advance(%{state | x: byte_size(packet)})}
  end

  # Load from scratch memory into X - X = M[k]
  defp step({:ldx, :mem, k}, %{mem: mem} = state) do
    {:continue, advance(%{state | x: Map.get(mem, k, 0)})}
  end

  # Load IP header length hack - X = 4 * (packet[k] & 0xF)
  defp step({:ldx, :msh, k}, %{packet: packet} = state) do
    case packet do
      <<_::binary-size(k), byte::8, _::binary>> ->
        {:continue, advance(%{state | x: (byte &&& 0x0F) * 4})}

      _ ->
        {:error, {:packet_too_short, k, 1}}
    end
  end

  # Store A to scratch memory - M[k] = A
  defp step({:st, k}, %{a: a, mem: mem} = state) do
    {:continue, advance(%{state | mem: Map.put(mem, k, a)})}
  end

  # Store X to scratch memory - M[k] = X
  defp step({:stx, k}, %{x: x, mem: mem} = state) do
    {:continue, advance(%{state | mem: Map.put(mem, k, x)})}
  end

  # ALU operations with constant
  defp step({:add, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: a + k &&& 0xFFFFFFFF})}
  end

  defp step({:sub, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: a - k &&& 0xFFFFFFFF})}
  end

  defp step({:mul, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: a * k &&& 0xFFFFFFFF})}
  end

  defp step({:div, k}, _state) when is_integer(k) and k == 0 do
    {:error, :division_by_zero}
  end

  defp step({:div, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: div(a, k)})}
  end

  defp step({:mod, k}, _state) when is_integer(k) and k == 0 do
    {:error, :division_by_zero}
  end

  defp step({:mod, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: rem(a, k)})}
  end

  defp step({:and, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: a &&& k})}
  end

  defp step({:or, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: a ||| k})}
  end

  defp step({:xor, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: bxor(a, k)})}
  end

  defp step({:lsh, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: a <<< k &&& 0xFFFFFFFF})}
  end

  defp step({:rsh, k}, %{a: a} = state) when is_integer(k) do
    {:continue, advance(%{state | a: a >>> k})}
  end

  # ALU operations with X register
  defp step({:add, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: a + x &&& 0xFFFFFFFF})}
  end

  defp step({:sub, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: a - x &&& 0xFFFFFFFF})}
  end

  defp step({:mul, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: a * x &&& 0xFFFFFFFF})}
  end

  defp step({:div, :x}, %{x: 0}) do
    {:error, :division_by_zero}
  end

  defp step({:div, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: div(a, x)})}
  end

  defp step({:mod, :x}, %{x: 0}) do
    {:error, :division_by_zero}
  end

  defp step({:mod, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: rem(a, x)})}
  end

  defp step({:and, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: a &&& x})}
  end

  defp step({:or, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: a ||| x})}
  end

  defp step({:xor, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: bxor(a, x)})}
  end

  defp step({:lsh, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: a <<< x &&& 0xFFFFFFFF})}
  end

  defp step({:rsh, :x}, %{a: a, x: x} = state) do
    {:continue, advance(%{state | a: a >>> x})}
  end

  # Negation - A = -A
  defp step(:neg, %{a: a} = state) do
    {:continue, advance(%{state | a: -a &&& 0xFFFFFFFF})}
  end

  # Unconditional jump
  defp step({:jmp, :ja, k}, %{pc: pc} = state) do
    {:continue, %{state | pc: pc + 1 + k}}
  end

  # Conditional jumps with constant
  defp step({:jmp, :jeq, :k, k, jt, jf}, %{a: a, pc: pc} = state) do
    offset = if a == k, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  defp step({:jmp, :jgt, :k, k, jt, jf}, %{a: a, pc: pc} = state) do
    offset = if a > k, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  defp step({:jmp, :jge, :k, k, jt, jf}, %{a: a, pc: pc} = state) do
    offset = if a >= k, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  defp step({:jmp, :jset, :k, k, jt, jf}, %{a: a, pc: pc} = state) do
    offset = if (a &&& k) != 0, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  # Conditional jumps with X register
  defp step({:jmp, :jeq, :x, jt, jf}, %{a: a, x: x, pc: pc} = state) do
    offset = if a == x, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  defp step({:jmp, :jgt, :x, jt, jf}, %{a: a, x: x, pc: pc} = state) do
    offset = if a > x, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  defp step({:jmp, :jge, :x, jt, jf}, %{a: a, x: x, pc: pc} = state) do
    offset = if a >= x, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  defp step({:jmp, :jset, :x, jt, jf}, %{a: a, x: x, pc: pc} = state) do
    offset = if (a &&& x) != 0, do: jt, else: jf
    {:continue, %{state | pc: pc + 1 + offset}}
  end

  # Return instructions
  defp step({:ret, :k, k}, _state), do: {:return, k}
  defp step({:ret, :a}, %{a: a}), do: {:return, a}

  # Misc instructions
  defp step(:tax, %{a: a} = state), do: {:continue, advance(%{state | x: a})}
  defp step(:txa, %{x: x} = state), do: {:continue, advance(%{state | a: x})}

  # Unknown instruction
  defp step(instruction, _state), do: {:error, {:unknown_instruction, instruction}}

  # Helper to advance PC
  defp advance(%{pc: pc} = state), do: %{state | pc: pc + 1}

  # Helper to load from packet into A
  defp load_packet(%{packet: packet} = state, offset, size) do
    case extract_bytes(packet, offset, size) do
      {:ok, value} -> {:continue, advance(%{state | a: value})}
      {:error, _} = error -> error
    end
  end

  # Helper to load from packet into X
  defp load_packet_x(%{packet: packet} = state, offset, size) do
    case extract_bytes(packet, offset, size) do
      {:ok, value} -> {:continue, advance(%{state | x: value})}
      {:error, _} = error -> error
    end
  end

  # Extract bytes from packet as big-endian integer
  defp extract_bytes(packet, offset, size) when offset >= 0 do
    bits = size * 8

    case packet do
      <<_::binary-size(offset), value::size(bits), _::binary>> ->
        {:ok, value}

      _ ->
        {:error, {:packet_too_short, offset, size}}
    end
  end

  defp extract_bytes(_packet, offset, _size) do
    {:error, {:negative_offset, offset}}
  end
end
