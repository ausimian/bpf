defmodule BPF do
  @moduledoc """
  A library for compiling Elixir binary pattern matching expressions into BPF programs.

  ## Example

      # Create a BPF program from a pattern matching expression
      prog = BPF.bpf(fn <<4::4, ihl::4, _::binary>> when ihl >= 5 -> true end)

      # Run it against a packet
      BPF.run(prog, <<0x45, 0x00, 0x00, 0x14>>)
      # => {:ok, 0xFFFFFFFF}

      # Convert to raw bytecode for kernel use
      bytes = BPF.Program.to_bytes(prog)
  """

  @doc """
  Compile a binary pattern matching function into a BPF program.

  Takes a quoted anonymous function with binary pattern matching and optional guards,
  and compiles it to a `%BPF.Program{}` struct.

  ## Examples

      # Simple byte match
      iex> prog = BPF.bpf(fn <<x::8>> when x == 42 -> true end)
      iex> BPF.run(prog, <<42>>)
      {:ok, 0xFFFFFFFF}

      # IPv4 packet filter
      iex> prog = BPF.bpf(fn <<4::4, ihl::4, _::binary>> when ihl >= 5 -> true end)
      iex> BPF.run(prog, <<0x45, 0x00, 0x00, 0x28>>)
      {:ok, 0xFFFFFFFF}

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
  """
  defmacro bpf(expr) do
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
  Run a BPF program against a binary packet.

  Returns `{:ok, return_value}` on success, or `{:error, reason}` on failure.

  ## Return Values

  - `0xFFFFFFFF` - Accept (pattern matched, guards passed)
  - `0` - Reject (no match)
  - Other values - Custom return from function body

  ## Examples

      iex> prog = BPF.bpf(fn <<x::8>> -> true end)
      iex> BPF.run(prog, <<42>>)
      {:ok, 0xFFFFFFFF}
  """
  defdelegate run(program, packet), to: BPF.Interpreter
end
