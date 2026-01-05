defmodule BPF.Program do
  @moduledoc """
  A compiled BPF program.

  Contains a list of instructions and optional metadata.
  """

  defstruct [:instructions, :bindings]

  @type t :: %__MODULE__{
          instructions: [BPF.Instruction.t()],
          bindings: %{atom() => non_neg_integer()}
        }

  @doc """
  Create a new BPF program from a list of instructions.
  """
  def new(instructions, bindings \\ %{}) do
    %__MODULE__{
      instructions: instructions,
      bindings: bindings
    }
  end

  @doc """
  Encode the program to raw BPF bytecode.

  Returns a binary suitable for use with SO_ATTACH_FILTER.
  """
  def to_bytes(%__MODULE__{instructions: instructions}) do
    instructions
    |> Enum.map(&BPF.Instruction.to_binary/1)
    |> IO.iodata_to_binary()
  end

  @doc """
  Returns the number of instructions in the program.
  """
  def length(%__MODULE__{instructions: instructions}) do
    Kernel.length(instructions)
  end
end
