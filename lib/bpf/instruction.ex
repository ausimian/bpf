defmodule BPF.Instruction do
  @moduledoc """
  Classic BPF instruction definitions and encoding.

  BPF has a simple instruction set with:
  - A: 32-bit accumulator
  - X: 32-bit index register
  - M[0-15]: 16 x 32-bit scratch memory slots

  Each instruction is represented as a tuple. Examples:
  - `{:ld, :b, [:k, 0]}` - load byte at offset 0 into A
  - `{:add, 4}` - add 4 to A
  - `{:jmp, :jeq, :k, 5, 1, 0}` - if A == 5, skip 1, else skip 0
  - `:txa` - copy X to A
  - `{:ret, :k, 0xFFFFFFFF}` - return constant
  """

  @type t() :: :atom | tuple()

  import Bitwise

  # Instruction class (upper 3 bits of opcode)
  @bpf_ld 0x00
  @bpf_ldx 0x01
  @bpf_st 0x02
  @bpf_stx 0x03
  @bpf_alu 0x04
  @bpf_jmp 0x05
  @bpf_ret 0x06
  @bpf_misc 0x07

  # Load size (bits 3-4 of opcode)
  # 32-bit word
  @bpf_w 0x00
  # 16-bit halfword
  @bpf_h 0x08
  # 8-bit byte
  @bpf_b 0x10

  # Load mode (bits 5-7 of opcode)
  # immediate value
  @bpf_imm 0x00
  # absolute offset in packet
  @bpf_abs 0x20
  # indirect: X + k
  @bpf_ind 0x40
  # scratch memory M[k]
  @bpf_mem 0x60
  # packet length
  @bpf_len 0x80
  # IP header length hack: 4 * (packet[k] & 0xF)
  @bpf_msh 0xA0

  # ALU operations (bits 4-7 of opcode)
  @bpf_add 0x00
  @bpf_sub 0x10
  @bpf_mul 0x20
  @bpf_div 0x30
  @bpf_or 0x40
  @bpf_and 0x50
  @bpf_lsh 0x60
  @bpf_rsh 0x70
  @bpf_neg 0x80
  @bpf_mod 0x90
  @bpf_xor 0xA0

  # Source operand for ALU/JMP
  # use constant k
  @bpf_k 0x00
  # use X register
  @bpf_x 0x08

  # Jump conditions
  # unconditional
  @bpf_ja 0x00
  # jump if equal
  @bpf_jeq 0x10
  # jump if greater than
  @bpf_jgt 0x20
  # jump if greater or equal
  @bpf_jge 0x30
  # jump if A & k != 0
  @bpf_jset 0x40

  # Misc operations
  # X = A
  @bpf_tax 0x00
  # A = X
  @bpf_txa 0x80

  # Return source
  # return constant
  @bpf_rval_k 0x00
  # return A
  @bpf_rval_a 0x10

  @doc """
  Encode an instruction tuple to its binary representation.

  Returns `{code, jt, jf, k}` where:
  - code: 16-bit opcode
  - jt: 8-bit jump-true offset
  - jf: 8-bit jump-false offset
  - k: 32-bit constant/offset
  """
  def encode(instruction)

  # Load instructions into A
  def encode({:ld, :w, [:k, k]}), do: {@bpf_ld ||| @bpf_w ||| @bpf_abs, 0, 0, k}
  def encode({:ld, :h, [:k, k]}), do: {@bpf_ld ||| @bpf_h ||| @bpf_abs, 0, 0, k}
  def encode({:ld, :b, [:k, k]}), do: {@bpf_ld ||| @bpf_b ||| @bpf_abs, 0, 0, k}

  def encode({:ld, :w, [:x, k]}), do: {@bpf_ld ||| @bpf_w ||| @bpf_ind, 0, 0, k}
  def encode({:ld, :h, [:x, k]}), do: {@bpf_ld ||| @bpf_h ||| @bpf_ind, 0, 0, k}
  def encode({:ld, :b, [:x, k]}), do: {@bpf_ld ||| @bpf_b ||| @bpf_ind, 0, 0, k}

  def encode({:ld, :imm, k}), do: {@bpf_ld ||| @bpf_imm, 0, 0, k}
  def encode({:ld, :len}), do: {@bpf_ld ||| @bpf_len, 0, 0, 0}
  def encode({:ld, :mem, k}), do: {@bpf_ld ||| @bpf_mem, 0, 0, k}

  # Load instructions into X
  def encode({:ldx, :w, [:k, k]}), do: {@bpf_ldx ||| @bpf_w ||| @bpf_abs, 0, 0, k}
  def encode({:ldx, :h, [:k, k]}), do: {@bpf_ldx ||| @bpf_h ||| @bpf_abs, 0, 0, k}
  def encode({:ldx, :b, [:k, k]}), do: {@bpf_ldx ||| @bpf_b ||| @bpf_abs, 0, 0, k}

  def encode({:ldx, :imm, k}), do: {@bpf_ldx ||| @bpf_imm, 0, 0, k}
  def encode({:ldx, :len}), do: {@bpf_ldx ||| @bpf_len, 0, 0, 0}
  def encode({:ldx, :mem, k}), do: {@bpf_ldx ||| @bpf_mem, 0, 0, k}
  def encode({:ldx, :msh, k}), do: {@bpf_ldx ||| @bpf_b ||| @bpf_msh, 0, 0, k}

  # Store instructions
  def encode({:st, k}), do: {@bpf_st, 0, 0, k}
  def encode({:stx, k}), do: {@bpf_stx, 0, 0, k}

  # ALU with constant
  def encode({:add, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_add ||| @bpf_k, 0, 0, k}
  def encode({:sub, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_sub ||| @bpf_k, 0, 0, k}
  def encode({:mul, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_mul ||| @bpf_k, 0, 0, k}
  def encode({:div, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_div ||| @bpf_k, 0, 0, k}
  def encode({:mod, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_mod ||| @bpf_k, 0, 0, k}
  def encode({:and, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_and ||| @bpf_k, 0, 0, k}
  def encode({:or, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_or ||| @bpf_k, 0, 0, k}
  def encode({:xor, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_xor ||| @bpf_k, 0, 0, k}
  def encode({:lsh, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_lsh ||| @bpf_k, 0, 0, k}
  def encode({:rsh, k}) when is_integer(k), do: {@bpf_alu ||| @bpf_rsh ||| @bpf_k, 0, 0, k}

  # ALU with X register
  def encode({:add, :x}), do: {@bpf_alu ||| @bpf_add ||| @bpf_x, 0, 0, 0}
  def encode({:sub, :x}), do: {@bpf_alu ||| @bpf_sub ||| @bpf_x, 0, 0, 0}
  def encode({:mul, :x}), do: {@bpf_alu ||| @bpf_mul ||| @bpf_x, 0, 0, 0}
  def encode({:div, :x}), do: {@bpf_alu ||| @bpf_div ||| @bpf_x, 0, 0, 0}
  def encode({:mod, :x}), do: {@bpf_alu ||| @bpf_mod ||| @bpf_x, 0, 0, 0}
  def encode({:and, :x}), do: {@bpf_alu ||| @bpf_and ||| @bpf_x, 0, 0, 0}
  def encode({:or, :x}), do: {@bpf_alu ||| @bpf_or ||| @bpf_x, 0, 0, 0}
  def encode({:xor, :x}), do: {@bpf_alu ||| @bpf_xor ||| @bpf_x, 0, 0, 0}
  def encode({:lsh, :x}), do: {@bpf_alu ||| @bpf_lsh ||| @bpf_x, 0, 0, 0}
  def encode({:rsh, :x}), do: {@bpf_alu ||| @bpf_rsh ||| @bpf_x, 0, 0, 0}

  # Negation (no operand)
  def encode(:neg), do: {@bpf_alu ||| @bpf_neg, 0, 0, 0}

  # Jump instructions with constant
  def encode({:jmp, :ja, k}), do: {@bpf_jmp ||| @bpf_ja, 0, 0, k}
  def encode({:jmp, :jeq, :k, k, jt, jf}), do: {@bpf_jmp ||| @bpf_jeq ||| @bpf_k, jt, jf, k}
  def encode({:jmp, :jgt, :k, k, jt, jf}), do: {@bpf_jmp ||| @bpf_jgt ||| @bpf_k, jt, jf, k}
  def encode({:jmp, :jge, :k, k, jt, jf}), do: {@bpf_jmp ||| @bpf_jge ||| @bpf_k, jt, jf, k}
  def encode({:jmp, :jset, :k, k, jt, jf}), do: {@bpf_jmp ||| @bpf_jset ||| @bpf_k, jt, jf, k}

  # Jump instructions with X register
  def encode({:jmp, :jeq, :x, jt, jf}), do: {@bpf_jmp ||| @bpf_jeq ||| @bpf_x, jt, jf, 0}
  def encode({:jmp, :jgt, :x, jt, jf}), do: {@bpf_jmp ||| @bpf_jgt ||| @bpf_x, jt, jf, 0}
  def encode({:jmp, :jge, :x, jt, jf}), do: {@bpf_jmp ||| @bpf_jge ||| @bpf_x, jt, jf, 0}
  def encode({:jmp, :jset, :x, jt, jf}), do: {@bpf_jmp ||| @bpf_jset ||| @bpf_x, jt, jf, 0}

  # Return instructions
  def encode({:ret, :k, k}), do: {@bpf_ret ||| @bpf_rval_k, 0, 0, k}
  def encode({:ret, :a}), do: {@bpf_ret ||| @bpf_rval_a, 0, 0, 0}

  # Misc instructions
  def encode(:tax), do: {@bpf_misc ||| @bpf_tax, 0, 0, 0}
  def encode(:txa), do: {@bpf_misc ||| @bpf_txa, 0, 0, 0}

  @doc """
  Encode an instruction to its 8-byte binary format.
  """
  def to_binary(instruction) do
    {code, jt, jf, k} = encode(instruction)
    <<code::16-little, jt::8, jf::8, k::32-little>>
  end
end
