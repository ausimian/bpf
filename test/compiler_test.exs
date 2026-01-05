defmodule BPF.CompilerTest do
  use ExUnit.Case, async: true

  alias BPF.{Parser, Compiler, Interpreter}

  describe "compile/1 basic patterns" do
    test "compiles simple byte binding" do
      ast = quote do: fn <<x::8>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Should accept any single-byte packet
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<42>>)
    end

    test "compiles literal matching" do
      ast = quote do: fn <<4::4, _::4>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Should accept packets starting with 0x4X
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x45>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x4F>>)

      # Should reject packets not starting with 0x4X
      assert {:ok, 0} = Interpreter.run(prog, <<0x60>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x35>>)
    end

    test "compiles multiple literal checks" do
      ast = quote do: fn <<4::4, 5::4>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Should only accept 0x45
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x45>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x46>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x55>>)
    end

    test "compiles byte-aligned literal" do
      ast = quote do: fn <<0x45::8, _::binary>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x45, 0x00, 0x00>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x60, 0x00, 0x00>>)
    end

    test "compiles halfword literal" do
      ast = quote do: fn <<0x0800::16, _::binary>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # EtherType 0x0800 = IPv4
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x08, 0x00, 0xAA>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x08, 0x06, 0xAA>>)
    end
  end

  describe "compile/1 guards" do
    test "compiles equality guard" do
      ast = quote do: fn <<x::8>> when x == 42 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<42>>)
      assert {:ok, 0} = Interpreter.run(prog, <<43>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0>>)
    end

    test "compiles inequality guard" do
      ast = quote do: fn <<x::8>> when x != 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<255>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0>>)
    end

    test "compiles greater than guard" do
      ast = quote do: fn <<x::8>> when x > 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<101>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<200>>)
      assert {:ok, 0} = Interpreter.run(prog, <<100>>)
      assert {:ok, 0} = Interpreter.run(prog, <<50>>)
    end

    test "compiles greater than or equal guard" do
      ast = quote do: fn <<x::8>> when x >= 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<100>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<101>>)
      assert {:ok, 0} = Interpreter.run(prog, <<99>>)
    end

    test "compiles less than guard" do
      ast = quote do: fn <<x::8>> when x < 50 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<49>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<50>>)
      assert {:ok, 0} = Interpreter.run(prog, <<100>>)
    end

    test "compiles less than or equal guard" do
      ast = quote do: fn <<x::8>> when x <= 50 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<50>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<51>>)
    end

    test "compiles logical AND guard" do
      ast = quote do: fn <<x::8, y::8>> when x > 0 and y > 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1, 1>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<100, 200>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0, 1>>)
      assert {:ok, 0} = Interpreter.run(prog, <<1, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0, 0>>)
    end

    test "compiles logical OR guard" do
      ast = quote do: fn <<x::8>> when x == 1 or x == 2 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<2>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<3>>)
    end

    test "compiles bitwise AND in guard" do
      ast = quote do: fn <<x::8>> when band(x, 0x0F) == 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Lower nibble should be 5
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x05>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x15>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xF5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x04>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x06>>)
    end

    test "compiles arithmetic in guard" do
      ast = quote do: fn <<x::8, y::8>> when x + y > 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<60, 50>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<101, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<50, 50>>)
      assert {:ok, 0} = Interpreter.run(prog, <<50, 40>>)
    end
  end

  describe "compile/1 combined patterns and guards" do
    test "compiles literal pattern with guard" do
      # Accept IPv4 packets (version == 4) with IHL >= 5
      ast = quote do: fn <<4::4, ihl::4, _::binary>> when ihl >= 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Valid IPv4 with IHL=5 (0x45)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x45, 0x00>>)

      # Valid IPv4 with IHL=15 (0x4F)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x4F, 0x00>>)

      # IPv4 with IHL=4 (invalid)
      assert {:ok, 0} = Interpreter.run(prog, <<0x44, 0x00>>)

      # IPv6 (version = 6)
      assert {:ok, 0} = Interpreter.run(prog, <<0x60, 0x00>>)
    end

    test "compiles complex IP header check" do
      # Accept if: version==4 AND ihl>=5 AND total_length>20
      ast =
        quote do
          fn <<4::4, ihl::4, _::8, len::16, _::binary>> when ihl >= 5 and len > 20 -> true end
        end

      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Valid: version=4, IHL=5, total_length=40 (0x0028)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x45, 0x00, 0x00, 0x28, 0x00>>)

      # Invalid: version=6
      assert {:ok, 0} = Interpreter.run(prog, <<0x65, 0x00, 0x00, 0x28, 0x00>>)

      # Invalid: IHL=4
      assert {:ok, 0} = Interpreter.run(prog, <<0x44, 0x00, 0x00, 0x28, 0x00>>)

      # Invalid: total_length=20 (0x0014)
      assert {:ok, 0} = Interpreter.run(prog, <<0x45, 0x00, 0x00, 0x14, 0x00>>)
    end
  end

  describe "compile/1 multiple clauses" do
    test "compiles multiple clauses with first-match semantics" do
      ast =
        quote do
          fn
            <<4::4, _::4, _::binary>> -> true
            <<6::4, _::4, _::binary>> -> true
          end
        end

      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Both IPv4 and IPv6 should be accepted
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x45, 0x00>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x60, 0x00>>)

      # Other versions should be rejected
      assert {:ok, 0} = Interpreter.run(prog, <<0x50, 0x00>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x00, 0x00>>)
    end
  end

  describe "bindings tracking" do
    test "tracks binding offsets in program" do
      ast = quote do: fn <<a::8, b::16, c::8>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert prog.bindings[:a] == 0
      assert prog.bindings[:b] == 8
      assert prog.bindings[:c] == 24
    end
  end

  describe "compile/1 sub-byte bindings" do
    test "compiles sub-byte bindings in guards" do
      # Both x and y are 4-bit values
      ast = quote do: fn <<x::4, y::4>> when x > y -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # 0xF5 = upper nibble 0xF (15), lower nibble 0x5 (5) -> 15 > 5 = true
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xF5>>)
      # 0x5F = upper nibble 0x5 (5), lower nibble 0xF (15) -> 5 > 15 = false
      assert {:ok, 0} = Interpreter.run(prog, <<0x5F>>)
      # 0x55 = both nibbles equal -> 5 > 5 = false
      assert {:ok, 0} = Interpreter.run(prog, <<0x55>>)
    end

    test "compiles 4-bit literal followed by 4-bit binding" do
      ast = quote do: fn <<0x0A::4, x::4>> when x == 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # 0xA5 = upper nibble 0xA (matches), lower nibble 0x5 (equals 5)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xA5>>)
      # 0xA6 = upper nibble 0xA (matches), lower nibble 0x6 (not equals 5)
      assert {:ok, 0} = Interpreter.run(prog, <<0xA6>>)
      # 0xB5 = upper nibble 0xB (doesn't match 0xA)
      assert {:ok, 0} = Interpreter.run(prog, <<0xB5>>)
    end
  end

  describe "compile/1 arithmetic guards" do
    test "compiles subtraction in guard" do
      ast = quote do: fn <<x::8, y::8>> when x - y > 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<50, 30>>)
      assert {:ok, 0} = Interpreter.run(prog, <<20, 15>>)
    end

    test "compiles multiplication in guard" do
      ast = quote do: fn <<x::8>> when x * 2 > 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<60>>)
      assert {:ok, 0} = Interpreter.run(prog, <<50>>)
      assert {:ok, 0} = Interpreter.run(prog, <<40>>)
    end

    test "compiles division in guard" do
      ast = quote do: fn <<x::8>> when div(x, 4) >= 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<40>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<44>>)
      assert {:ok, 0} = Interpreter.run(prog, <<36>>)
    end
  end

  describe "compile/1 bitwise guards" do
    test "compiles bor in guard" do
      ast = quote do: fn <<x::8>> when bor(x, 0xF0) == 0xFF -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # 0x0F | 0xF0 = 0xFF
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x0F>>)
      # 0x0E | 0xF0 = 0xFE
      assert {:ok, 0} = Interpreter.run(prog, <<0x0E>>)
    end

    test "compiles bxor in guard" do
      ast = quote do: fn <<x::8>> when bxor(x, 0xFF) == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # 0xFF ^ 0xFF = 0
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xFF>>)
      # 0xFE ^ 0xFF = 0x01
      assert {:ok, 0} = Interpreter.run(prog, <<0xFE>>)
    end

    test "compiles bitwise and with binding on right" do
      ast = quote do: fn <<x::8, y::8>> when band(x, y) == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # 0xF0 & 0x0F = 0
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xF0, 0x0F>>)
      # 0xF0 & 0xFF = 0xF0
      assert {:ok, 0} = Interpreter.run(prog, <<0xF0, 0xFF>>)
    end
  end

  describe "compile/1 complex logical expressions" do
    test "compiles deeply nested AND" do
      ast = quote do: fn <<a::8, b::8, c::8>> when a > 0 and b > 0 and c > 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1, 2, 3>>)
      assert {:ok, 0} = Interpreter.run(prog, <<1, 2, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<1, 0, 3>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0, 2, 3>>)
    end

    test "compiles deeply nested OR" do
      ast = quote do: fn <<x::8>> when x == 1 or x == 2 or x == 3 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<2>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<3>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<4>>)
    end

    test "compiles mixed AND/OR expressions" do
      # (a > 0 and b > 0) or c > 100
      ast = quote do: fn <<a::8, b::8, c::8>> when (a > 0 and b > 0) or c > 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Both a and b positive
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1, 1, 0>>)
      # c > 100
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0, 0, 150>>)
      # Neither condition met
      assert {:ok, 0} = Interpreter.run(prog, <<0, 1, 50>>)
    end
  end

  describe "compile/1 action types" do
    test "compiles false action to reject" do
      ast = quote do: fn <<_::8>> -> false end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0} = Interpreter.run(prog, <<42>>)
    end

    test "compiles integer action to accept with value" do
      ast = quote do: fn <<_::8>> -> 65535 end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 65535} = Interpreter.run(prog, <<42>>)
    end

    test "compiles large integer action" do
      ast = quote do: fn <<_::8>> -> 0xFFFFFFFE end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFE} = Interpreter.run(prog, <<42>>)
    end

    test "compiles :accept atom to accept" do
      ast = quote do: fn <<_::8>> -> :accept end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<42>>)
    end

    test "compiles :reject atom to reject" do
      ast = quote do: fn <<_::8>> -> :reject end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0} = Interpreter.run(prog, <<42>>)
    end
  end

  describe "compile/1 16-bit and 32-bit bindings" do
    test "compiles 16-bit binding with guard" do
      ast = quote do: fn <<x::16>> when x > 1000 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # 0x0400 = 1024
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x04, 0x00>>)
      # 0x03E7 = 999
      assert {:ok, 0} = Interpreter.run(prog, <<0x03, 0xE7>>)
    end

    test "compiles 32-bit binding with guard" do
      ast = quote do: fn <<x::32>> when x == 0x12345678 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x12, 0x34, 0x56, 0x78>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x12, 0x34, 0x56, 0x79>>)
    end

    test "compiles multiple different sized bindings" do
      ast = quote do: fn <<a::8, b::16, c::8>> when a + c > b -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # a=100, b=50 (0x0032), c=100 -> 100 + 100 > 50 = true
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<100, 0x00, 0x32, 100>>)
      # a=10, b=1000 (0x03E8), c=10 -> 10 + 10 > 1000 = false
      assert {:ok, 0} = Interpreter.run(prog, <<10, 0x03, 0xE8, 10>>)
    end
  end

  describe "compile/1 comparison edge cases" do
    test "compiles binding on right side of comparison" do
      ast = quote do: fn <<x::8, y::8>> when x > y -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10, 5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 5>>)
    end

    test "compiles comparison with zero" do
      ast = quote do: fn <<x::8>> when x == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<1>>)
    end

    test "compiles comparison with max byte value" do
      ast = quote do: fn <<x::8>> when x == 255 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<255>>)
      assert {:ok, 0} = Interpreter.run(prog, <<254>>)
    end
  end

  describe "compile/1 empty and minimal patterns" do
    test "compiles pattern with only skip" do
      ast = quote do: fn <<_::binary>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1, 2, 3>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)
    end

    test "compiles pattern with fixed skip and binding" do
      ast = quote do: fn <<_::16, x::8>> when x > 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0, 0, 5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0, 0, 0>>)
    end

    test "compiles empty binary pattern" do
      ast = quote do: fn <<>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Empty pattern matches any packet
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<1, 2, 3>>)
    end
  end

  describe "compile/1 literal edge cases" do
    test "compiles literal value of 0" do
      ast = quote do: fn <<0::8>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<1>>)
    end

    test "compiles max byte literal" do
      ast = quote do: fn <<255::8>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<255>>)
      assert {:ok, 0} = Interpreter.run(prog, <<254>>)
    end

    test "compiles 32-bit literal" do
      ast = quote do: fn <<0xDEADBEEF::32>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xDE, 0xAD, 0xBE, 0xEF>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0xDE, 0xAD, 0xBE, 0xEE>>)
    end

    test "compiles multiple consecutive literals" do
      ast = quote do: fn <<0xAB::8, 0xCD::8, 0xEF::8>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xAB, 0xCD, 0xEF>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0xAB, 0xCD, 0x00>>)
    end
  end

  describe "compile/1 guard with bnot" do
    test "compiles bnot in guard operand" do
      # bnot(0) == -1 in signed, or 0xFFFFFFFF in unsigned
      ast = quote do: fn <<x::8>> when bnot(x) != 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # bnot(0xFF) = 0xFFFFFF00, which != 0
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xFF>>)
      # bnot(0) = 0xFFFFFFFF, which != 0
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0>>)
    end
  end

  describe "compile/1 program structure" do
    test "generates final reject fallthrough" do
      ast = quote do: fn <<1::8>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # If pattern doesn't match, should fallthrough to reject
      assert {:ok, 0} = Interpreter.run(prog, <<2>>)
    end

    test "handles clause with no guard and no literal" do
      ast = quote do: fn <<x::8>> -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Any single byte should be accepted
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<255>>)
    end
  end

  describe "compile/1 scratch memory usage" do
    test "handles many bindings in pattern" do
      # Test that multiple bindings are properly allocated to scratch memory
      ast = quote do: fn <<a::8, b::8, c::8, d::8, e::8>> when a + b + c + d + e > 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<30, 30, 30, 30, 30>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 10, 10, 10, 10>>)
    end

    test "reuses scratch slot for same binding" do
      ast = quote do: fn <<x::8>> when x > 0 and x < 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<50>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<100>>)
    end
  end

  describe "compile/1 multiple clause ordering" do
    test "first matching clause wins" do
      ast =
        quote do
          fn
            <<1::8, _::binary>> -> 100
            <<_::8, 2::8, _::binary>> -> 200
            <<_::binary>> -> 300
          end
        end

      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # First clause matches
      assert {:ok, 100} = Interpreter.run(prog, <<1, 2, 3>>)
      # Second clause matches (first doesn't)
      assert {:ok, 200} = Interpreter.run(prog, <<0, 2, 3>>)
      # Third clause (catch-all) matches
      assert {:ok, 300} = Interpreter.run(prog, <<0, 0, 3>>)
    end

    test "later clause checked if earlier fails guard" do
      ast =
        quote do
          fn
            <<x::8>> when x > 100 -> 1
            <<x::8>> when x > 50 -> 2
            <<_::8>> -> 3
          end
        end

      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 1} = Interpreter.run(prog, <<150>>)
      assert {:ok, 2} = Interpreter.run(prog, <<75>>)
      assert {:ok, 3} = Interpreter.run(prog, <<25>>)
    end
  end

  describe "compile/1 arithmetic with bindings" do
    test "subtraction with binding on right" do
      ast = quote do: fn <<x::8, y::8>> when x - y > 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<20, 10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 8>>)
    end

    test "multiplication with binding on right" do
      ast = quote do: fn <<x::8, y::8>> when x * y > 50 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10, 6>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 5>>)
    end

    test "division with binding on right" do
      ast = quote do: fn <<x::8, y::8>> when div(x, y) > 2 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<30, 10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 10>>)
    end
  end

  describe "compile/1 bitwise with bindings" do
    test "bor with binding on right" do
      ast = quote do: fn <<x::8, y::8>> when bor(x, y) == 0xFF -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xF0, 0x0F>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0xF0, 0x00>>)
    end

    test "bxor with binding on right" do
      ast = quote do: fn <<x::8, y::8>> when bxor(x, y) == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xAB, 0xAB>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0xAB, 0xCD>>)
    end
  end

  describe "compile/1 OR with binding comparisons" do
    test "OR with binding on right side of comparison" do
      ast = quote do: fn <<x::8, y::8>> when x == y or x == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0, 10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 10>>)
    end

    test "OR with all comparison types using bindings" do
      ast = quote do: fn <<x::8, y::8>> when x > y or x < 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10, 5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<3, 10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 10>>)
    end

    test "OR with inequality using binding" do
      ast = quote do: fn <<x::8, y::8>> when x != y or x == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 10>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 5>>)
    end

    test "OR with >= using binding" do
      ast = quote do: fn <<x::8, y::8>> when x >= y or y == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10, 5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 10>>)
    end

    test "OR with <= using binding" do
      ast = quote do: fn <<x::8, y::8>> when x <= y or y == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 10>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 5>>)
    end
  end

  describe "compile/1 comparison with X register" do
    test "equality with binding" do
      ast = quote do: fn <<x::8, y::8>> when x == y -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<42, 42>>)
      assert {:ok, 0} = Interpreter.run(prog, <<42, 43>>)
    end

    test "inequality with binding" do
      ast = quote do: fn <<x::8, y::8>> when x != y -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<42, 43>>)
      assert {:ok, 0} = Interpreter.run(prog, <<42, 42>>)
    end

    test "greater than or equal with binding" do
      ast = quote do: fn <<x::8, y::8>> when x >= y -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10, 5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 10>>)
    end

    test "less than with binding" do
      ast = quote do: fn <<x::8, y::8>> when x < y -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 5>>)
    end

    test "less than or equal with binding" do
      ast = quote do: fn <<x::8, y::8>> when x <= y -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 10>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 5>>)
    end
  end

  describe "compile/1 complex nested expressions" do
    test "arithmetic expression as comparison operand" do
      # (x + y) > (a + b)
      ast = quote do: fn <<x::8, y::8, a::8, b::8>> when x + y > a + b -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10, 10, 5, 5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 5, 10, 10>>)
    end

    test "bitwise expression in comparison" do
      # band(x, y) == band(a, b)
      ast = quote do: fn <<x::8, y::8, a::8, b::8>> when band(x, y) == band(a, b) -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # band(0xFF, 0x0F) = 0x0F, band(0x1F, 0x0F) = 0x0F - equal, should accept
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xFF, 0x0F, 0x1F, 0x0F>>)
      # band(0xFF, 0x0F) = 0x0F, band(0xF0, 0x0F) = 0x00 - not equal, should reject
      assert {:ok, 0} = Interpreter.run(prog, <<0xFF, 0x0F, 0xF0, 0x0F>>)
    end
  end

  describe "compile/1 sub-byte binding at lower nibble" do
    test "binding in lower nibble position" do
      # <<_::4, x::4>> - x is in the lower nibble (no shift needed)
      ast = quote do: fn <<_::4, x::4>> when x == 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xA5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0xF5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0xA6>>)
    end
  end

  # These tests exercise compile_comparison_jump_inverted with :k source
  # (used for OR branches with literal comparisons)
  describe "compile/1 OR with literal comparisons (inverted jumps)" do
    test "OR with neq literal" do
      ast = quote do: fn <<x::8>> when x != 5 or x != 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # x != 5 is true for all except 5
      # x != 10 is true for all except 10
      # OR: true unless x is both equal to 5 AND equal to 10 (impossible)
      # So this should accept everything
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<20>>)
    end

    test "OR with gt literal" do
      ast = quote do: fn <<x::8>> when x > 10 or x == 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<15>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<8>>)
    end

    test "OR with gte literal" do
      ast = quote do: fn <<x::8>> when x >= 10 or x == 5 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<15>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<8>>)
    end

    test "OR with lt literal" do
      ast = quote do: fn <<x::8>> when x < 10 or x == 20 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<20>>)
      assert {:ok, 0} = Interpreter.run(prog, <<15>>)
    end

    test "OR with lte literal" do
      ast = quote do: fn <<x::8>> when x <= 10 or x == 20 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<20>>)
      assert {:ok, 0} = Interpreter.run(prog, <<15>>)
    end
  end

  describe "compile/1 OR with X register comparisons" do
    test "OR with lt using binding (x register)" do
      # This exercises compile_comparison_jump_inverted with :x and :lt
      ast = quote do: fn <<x::8, y::8>> when x < y or x == 0 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 10>>)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0, 5>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 5>>)
    end
  end

  describe "compile/1 arith_to_alu functions" do
    test "arithmetic with literal uses arith_to_alu" do
      ast = quote do: fn <<x::8>> when x + 5 > 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5>>)
    end

    test "subtraction with literal uses arith_to_alu" do
      ast = quote do: fn <<x::8>> when x - 5 > 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<20>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10>>)
    end

    test "multiplication with literal uses arith_to_alu" do
      ast = quote do: fn <<x::8>> when x * 2 > 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<10>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5>>)
    end

    test "division with literal uses arith_to_alu" do
      ast = quote do: fn <<x::8>> when div(x, 2) > 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<30>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10>>)
    end
  end

  describe "compile/1 register allocation optimization" do
    test "eliminates redundant load after binding store" do
      # This pattern used to generate: st 0, ld mem 0, jeq
      # Now it should generate: st 0, jeq (skipping the redundant load)
      ast = quote do: fn <<4::4, flags::4, _::binary>> when flags == 8 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Verify the program works correctly
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<0x48, 0x00>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x49, 0x00>>)
      assert {:ok, 0} = Interpreter.run(prog, <<0x58, 0x00>>)

      # Verify there's no {:ld, :mem, 0} immediately after {:st, 0}
      # (i.e., the redundant load was eliminated)
      instrs = prog.instructions

      st_indices =
        Enum.with_index(instrs)
        |> Enum.filter(fn {i, _} -> match?({:st, _}, i) end)
        |> Enum.map(&elem(&1, 1))

      for st_idx <- st_indices do
        next_instr = Enum.at(instrs, st_idx + 1)
        # The instruction after st should NOT be ld mem with the same slot
        {_, st_slot} = Enum.at(instrs, st_idx)

        refute match?({:ld, :mem, ^st_slot}, next_instr),
               "Found redundant {:ld, :mem, #{st_slot}} immediately after {:st, #{st_slot}}"
      end
    end

    test "reuses value in A when binding used multiple times" do
      # When a binding is used twice, second use should reuse A if possible
      ast = quote do: fn <<x::8, y::8>> when x > 5 and x < 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Verify correctness
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<7, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<5, 0>>)
      assert {:ok, 0} = Interpreter.run(prog, <<10, 0>>)
    end

    test "loads from scratch when A was clobbered" do
      # When another operation clobbers A, we need to reload from scratch
      ast = quote do: fn <<x::8, y::8>> when x + y > 10 and x > 3 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Verify correctness: x + y > 10 AND x > 3
      # 5+10=15>10, 5>3
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, <<5, 10>>)
      # 3+10=13>10, but 3>3 is false
      assert {:ok, 0} = Interpreter.run(prog, <<3, 10>>)
      # 5+2=7, not >10
      assert {:ok, 0} = Interpreter.run(prog, <<5, 2>>)
    end
  end

  describe "byte_size(packet) support" do
    test "byte_size(packet) compiles to packet length instruction" do
      # Using = packet to bind the full packet
      ast = quote do: fn <<_::binary>> = packet when byte_size(packet) > 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Verify it uses {:ld, :len}
      assert Enum.any?(prog.instructions, fn i -> i == {:ld, :len} end)

      # Verify correctness
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, :binary.copy(<<0>>, 101))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 100))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 50))
    end

    test "byte_size(packet) works with equality comparison" do
      ast = quote do: fn <<_::binary>> = packet when byte_size(packet) == 64 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, :binary.copy(<<0>>, 64))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 63))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 65))
    end

    test "byte_size(packet) works with less than comparison" do
      ast = quote do: fn <<_::binary>> = packet when byte_size(packet) < 50 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, :binary.copy(<<0>>, 49))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 50))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 100))
    end

    test "byte_size(packet) works combined with pattern and guards" do
      # Check IP version and packet length
      ast =
        quote do: fn <<4::4, _::4, _::binary>> = packet when byte_size(packet) >= 20 -> true end

      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # IPv4 header (version 4), 20+ bytes
      ipv4_header = <<0x45>> <> :binary.copy(<<0>>, 19)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, ipv4_header)

      # IPv4 but too short
      assert {:ok, 0} = Interpreter.run(prog, <<0x45, 0x00>>)

      # Long enough but wrong version
      assert {:ok, 0} = Interpreter.run(prog, <<0x60>> <> :binary.copy(<<0>>, 19))
    end

    test "byte_size(packet) works with reverse binding syntax" do
      # packet = <<_::binary>> (reverse order)
      ast = quote do: fn packet = <<_::binary>> when byte_size(packet) > 10 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, :binary.copy(<<0>>, 11))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 10))
    end

    test "byte_size on non-packet variable is not recognized as packet_len" do
      # byte_size(other_var) should NOT be treated as packet length
      # Since 'other' is not the packet variable bound with =, this should fail to parse
      # or be treated as a regular binding
      ast = quote do: fn <<x::8, _::binary>> = packet when byte_size(x) > 0 -> true end
      result = Parser.parse(ast)
      # x is not a binary, so byte_size(x) won't match the packet_len pattern
      # It will be treated as a binding reference, which should error
      assert {:error, _} = result
    end

    test "byte_size(packet) works in arithmetic expressions" do
      ast = quote do: fn <<_::binary>> = packet when byte_size(packet) - 14 > 100 -> true end
      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      # Need packet size > 114 (100 + 14)
      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, :binary.copy(<<0>>, 115))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 114))
    end

    test "byte_size(packet) works in complex boolean expressions" do
      ast =
        quote do: fn <<_::binary>> = packet
                     when byte_size(packet) > 50 and byte_size(packet) < 1500 ->
                true
              end

      {:ok, clauses} = Parser.parse(ast)
      {:ok, prog} = Compiler.compile(clauses)

      assert {:ok, 0xFFFFFFFF} = Interpreter.run(prog, :binary.copy(<<0>>, 100))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 50))
      assert {:ok, 0} = Interpreter.run(prog, :binary.copy(<<0>>, 1500))
    end

    test "without packet binding, byte_size of undefined var fails" do
      # Without = packet, byte_size(packet) refers to undefined binding
      ast = quote do: fn <<_::binary>> when byte_size(packet) > 0 -> true end
      # This should fail during parsing because 'packet' is not the packet variable
      # and byte_size of a non-packet variable is not supported
      result = Parser.parse(ast)
      assert {:error, {:invalid_operand, _}} = result
    end
  end
end
