defmodule BPF do
  @moduledoc """
  A library for compiling Elixir binary pattern matching expressions into classic BPF programs.

  Compiling produces a `%BPF.Program{}` struct containing the BPF instructions
  and variable bindings. The program can be interpreted against binary packets (for testing),
  or assembled into raw BPF bytecode for use with sockets or libpcap.

  ## Example

      iex> prog = BPF.compile(fn <<4::4, ihl::4, _::binary>> when ihl >= 5 -> true end)
      %BPF.Program{
        instructions: [
          {:ld, :b, [:k, 0]},
          {:rsh, 4},
          {:and, 15},
          {:jmp, :jeq, :k, 4, 0, 4},
          {:ld, :b, [:k, 0]},
          {:and, 15},
          {:jmp, :jge, :k, 5, 0, 1},
          {:ret, :k, 4294967295},
          {:ret, :k, 0}
        ],
        bindings: %{ihl: 4}
      }
      iex> BPF.interpret(prog, <<0x45, 0x00, 0x00, 0x14>>)
      true
      iex> BPF.assemble(prog)
      <<48, 0, 0, 0, 0, 0, 0, 0, 116, 0, 0, 0, 4, 0, 0, 0, 84, 0, 0, 0, 15, 0, 0, 0,
        21, 0, 0, 4, 4, 0, 0, 0, 48, 0, 0, 0, 0, 0, 0, 0, 84, 0, 0, 0, 15, 0, 0, 0,
        53, 0, 0, 1, 5, 0, 0, 0, 6, 0, 0, 0, 255, 255, 255, 255, 6, 0, 0, 0, 0, 0, 0,
        0>>
  """

  @doc """
  Compile a binary pattern matching function into a BPF program.

  Takes a quoted anonymous function with binary pattern matching and optional guards,
  and compiles it to a `%BPF.Program{}` struct.

  ## Examples

      # Simple byte match - returns a BPF.Program struct
      iex> BPF.compile(fn <<x::8>> when x == 42 -> true end)
      %BPF.Program{
        instructions: [
          {:ld, :b, [:k, 0]},
          {:jmp, :jeq, :k, 42, 0, 1},
          {:ret, :k, 4294967295},
          {:ret, :k, 0}
        ],
        bindings: %{x: 0}
      }

      # IPv4 packet filter
      iex> prog = BPF.compile(fn <<4::4, ihl::4, _::binary>> when ihl >= 5 -> true end)
      %BPF.Program{
        instructions: [
          {:ld, :b, [:k, 0]},
          {:rsh, 4},
          {:and, 15},
          {:jmp, :jeq, :k, 4, 0, 4},
          {:ld, :b, [:k, 0]},
          {:and, 15},
          {:jmp, :jge, :k, 5, 0, 1},
          {:ret, :k, 4294967295},
          {:ret, :k, 0}
        ],
        bindings: %{ihl: 4}
      }
      iex> BPF.interpret(prog, <<0x45, 0x00, 0x00, 0x28>>)
      true

      # Multi-clause: accept IPv4 or IPv6 packets over 100 bytes
      # IPv4: version=4, total length at bytes 2-3
      # IPv6: version=6, payload length at bytes 4-5 (add 40 for header)
      iex> prog = BPF.compile(fn
      ...>   <<4::4, _ihl::4, _tos::8, len::16, _::binary>> when len > 100 -> true
      ...>   <<6::4, _::28, len::16, _::binary>> when len + 40 > 100 -> true
      ...> end)
      %BPF.Program{
        instructions: [
          {:ld, :b, [:k, 0]},
          {:rsh, 4},
          {:and, 15},
          {:jmp, :jeq, :k, 4, 0, 3},
          {:ld, :h, [:k, 2]},
          {:jmp, :jgt, :k, 100, 0, 1},
          {:ret, :k, 4294967295},
          {:ld, :b, [:k, 0]},
          {:rsh, 4},
          {:and, 15},
          {:jmp, :jeq, :k, 6, 0, 4},
          {:ld, :h, [:k, 4]},
          {:add, 40},
          {:jmp, :jgt, :k, 100, 0, 1},
          {:ret, :k, 4294967295},
          {:ret, :k, 0}
        ],
        bindings: %{len: 32, _ihl: 4, _tos: 8}
      }
      iex> BPF.interpret(prog, <<0x45, 0x00, 0x00, 0x96, 0x00>>)  # IPv4, length 150
      true
      iex> BPF.interpret(prog, <<0x60, 0x00, 0x00, 0x00, 0x00, 0x50>>)  # IPv6, payload 80
      true
      iex> BPF.interpret(prog, <<0x45, 0x00, 0x00, 0x32, 0x00>>)  # IPv4, length 50
      false

      # Single clause with conditional return: accept IPv4 or IPv6 packets
      iex> prog = BPF.compile(fn <<version::4, _::binary>> -> version == 4 or version == 6 end)
      %BPF.Program{
        instructions: [
          {:ld, :b, [:k, 0]},
          {:rsh, 4},
          {:and, 15},
          {:st, 0},
          {:jmp, :jeq, :k, 4, 1, 0},
          {:jmp, :jeq, :k, 6, 0, 1},
          {:ret, :k, 4294967295},
          {:ret, :k, 0}
        ],
        bindings: %{version: 0}
      }
      iex> BPF.interpret(prog, <<0x45, 0x00>>)  # IPv4
      true
      iex> BPF.interpret(prog, <<0x60, 0x00>>)  # IPv6
      true
      iex> BPF.interpret(prog, <<0x50, 0x00>>)  # version 5
      false

      # Filter by packet length using byte_size/1
      # The packet variable must be bound with = to use byte_size
      iex> prog = BPF.compile(fn <<_::binary>> = packet when byte_size(packet) >= 64 -> true end)
      %BPF.Program{
        instructions: [
          {:ld, :len},
          {:jmp, :jge, :k, 64, 0, 1},
          {:ret, :k, 4294967295},
          {:ret, :k, 0}
        ],
        bindings: %{}
      }
      iex> BPF.interpret(prog, :binary.copy(<<0>>, 64))
      true
      iex> BPF.interpret(prog, :binary.copy(<<0>>, 63))
      false

      # Combine pattern matching with packet length check
      iex> prog = BPF.compile(fn <<4::4, _::4, _::binary>> = pkt when byte_size(pkt) >= 20 -> true end)
      %BPF.Program{
        instructions: [
          {:ld, :b, [:k, 0]},
          {:rsh, 4},
          {:and, 15},
          {:jmp, :jeq, :k, 4, 0, 3},
          {:ld, :len},
          {:jmp, :jge, :k, 20, 0, 1},
          {:ret, :k, 4294967295},
          {:ret, :k, 0}
        ],
        bindings: %{}
      }
      iex> BPF.interpret(prog, <<0x45>> <> :binary.copy(<<0>>, 19))  # IPv4, 20 bytes
      true
      iex> BPF.interpret(prog, <<0x45, 0x00>>)  # IPv4, but too short
      false

  ## Supported Patterns

  - Literal values: `<<4::4, 0x0800::16>>`
  - Bindings: `<<version::4, ihl::4, tos::8>>`
  - Skips: `<<_::8, _::binary>>`
  - Size modifiers: `<<x::16-little>>`

  ## Supported Guards

  - Comparisons: `==`, `!=`, `<`, `>`, `<=`, `>=`
  - Logical: `and`, `or`
  - Bitwise: `band/2`, `bor/2`, `bxor/2`
  - Arithmetic: `+`, `-`, `*`, `div/2`
  - Packet length: `byte_size(packet)` when `packet` is bound with `= packet`
  """
  defmacro compile(expr) do
    quote do
      case BPF.Parser.parse(unquote(Macro.escape(expr))) do
        {:ok, clauses} ->
          {:ok, program} = BPF.Compiler.compile(clauses)
          program

        {:error, reason} ->
          raise ArgumentError, "BPF parsing failed: #{inspect(reason)}"
      end
    end
  end

  @doc """
  Interpret a BPF program against a binary packet.

  Returns `true` if the program accepts the packet (non-zero return value),
  or `false` if it rejects (zero return value).

  ## Examples

      iex> prog = BPF.compile(fn <<x::8>> when x == 42 -> true end)
      iex> BPF.interpret(prog, <<42>>)
      true

      iex> prog = BPF.compile(fn <<x::8>> when x == 42 -> true end)
      iex> BPF.interpret(prog, <<99>>)
      false
  """
  def interpret(program, packet) do
    case BPF.Interpreter.run(program, packet) do
      {:ok, 0} -> false
      {:ok, _} -> true
    end
  end

  @doc """
  Assemble a BPF program into raw bytecode.

  Returns a binary suitable for use with `SO_ATTACH_FILTER` or other
  kernel interfaces that accept classic BPF bytecode.

  Each instruction is encoded as 8 bytes in little-endian format:
  - 2 bytes: opcode
  - 1 byte: jt (jump if true offset)
  - 1 byte: jf (jump if false offset)
  - 4 bytes: k (immediate constant)

  ## Examples

      iex> prog = BPF.compile(fn <<x::8>> when x == 42 -> true end)
      iex> bytes = BPF.assemble(prog)
      iex> is_binary(bytes) and rem(byte_size(bytes), 8) == 0
      true
  """
  defdelegate assemble(program), to: BPF.Program
end
