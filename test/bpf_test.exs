defmodule BPFTest do
  use ExUnit.Case, async: true

  import BPF

  describe "bpf/1 macro" do
    test "compiles simple pattern" do
      prog = bpf(fn <<x::8>> -> true end)
      assert %BPF.Program{} = prog
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<42>>)
    end

    test "compiles literal pattern" do
      prog = bpf(fn <<0x45::8, _::binary>> -> true end)
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x45, 0x00>>)
      assert {:ok, 0} = BPF.run(prog, <<0x60, 0x00>>)
    end

    test "compiles guard expression" do
      prog = bpf(fn <<x::8>> when x == 42 -> true end)
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<42>>)
      assert {:ok, 0} = BPF.run(prog, <<43>>)
    end

    test "compiles IPv4 header check" do
      prog = bpf(fn <<4::4, ihl::4, _::binary>> when ihl >= 5 -> true end)

      # Valid IPv4: version=4, IHL=5
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x45, 0x00>>)

      # Valid IPv4: version=4, IHL=15
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x4F, 0x00>>)

      # Invalid: version=4, IHL=4
      assert {:ok, 0} = BPF.run(prog, <<0x44, 0x00>>)

      # Invalid: version=6
      assert {:ok, 0} = BPF.run(prog, <<0x60, 0x00>>)
    end

    test "compiles multiple clauses" do
      prog =
        bpf(fn
          <<4::4, _::4, _::binary>> -> true
          <<6::4, _::4, _::binary>> -> true
        end)

      # Both IPv4 and IPv6 should match
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x45, 0x00>>)
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x60, 0x00>>)

      # Other versions should not match
      assert {:ok, 0} = BPF.run(prog, <<0x50, 0x00>>)
    end

    test "compiles complex guard with AND" do
      prog =
        bpf(fn <<4::4, ihl::4, _::8, len::16, _::binary>> when ihl >= 5 and len > 20 -> true end)

      # Valid: version=4, IHL=5, len=40
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x45, 0x00, 0x00, 0x28>>)

      # Invalid: len=20
      assert {:ok, 0} = BPF.run(prog, <<0x45, 0x00, 0x00, 0x14>>)

      # Invalid: IHL=4
      assert {:ok, 0} = BPF.run(prog, <<0x44, 0x00, 0x00, 0x28>>)
    end

    test "compiles guard with OR" do
      prog = bpf(fn <<x::8>> when x == 1 or x == 2 -> true end)

      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<1>>)
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<2>>)
      assert {:ok, 0} = BPF.run(prog, <<3>>)
    end

    test "compiles bitwise AND in guard" do
      # Match if lower nibble is 5
      prog = bpf(fn <<x::8>> when band(x, 0x0F) == 5 -> true end)

      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x05>>)
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0x45>>)
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<0xF5>>)
      assert {:ok, 0} = BPF.run(prog, <<0x04>>)
    end

    test "compiles arithmetic in guard" do
      prog = bpf(fn <<x::8, y::8>> when x + y > 100 -> true end)

      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, <<60, 50>>)
      assert {:ok, 0} = BPF.run(prog, <<50, 50>>)
    end
  end

  describe "BPF.Program.to_bytes/1" do
    test "serializes program to bytecode" do
      prog = bpf(fn <<x::8>> when x == 42 -> true end)
      bytes = BPF.Program.to_bytes(prog)

      assert is_binary(bytes)
      # Each instruction is 8 bytes
      assert rem(byte_size(bytes), 8) == 0
    end
  end

  describe "end-to-end examples" do
    test "TCP SYN packet filter" do
      # Accept TCP packets with SYN flag (bit 1) set
      # Assuming TCP flags at offset 33 in the packet
      prog =
        bpf(fn <<_::binary-size(33), flags::8, _::binary>> when band(flags, 0x02) != 0 ->
          true
        end)

      # SYN flag set
      syn_packet = :binary.copy(<<0>>, 33) <> <<0x02>>
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, syn_packet)

      # ACK flag set (no SYN)
      ack_packet = :binary.copy(<<0>>, 33) <> <<0x10>>
      assert {:ok, 0} = BPF.run(prog, ack_packet)
    end

    test "EtherType IPv4 filter" do
      # Match Ethernet frames with EtherType 0x0800 (IPv4)
      # EtherType is at offset 12-13 in Ethernet frame
      prog = bpf(fn <<_::binary-size(12), 0x0800::16, _::binary>> -> true end)

      # IPv4 frame
      ipv4_frame = :binary.copy(<<0>>, 12) <> <<0x08, 0x00, 0x45>>
      assert {:ok, 0xFFFFFFFF} = BPF.run(prog, ipv4_frame)

      # ARP frame (0x0806)
      arp_frame = :binary.copy(<<0>>, 12) <> <<0x08, 0x06, 0x00>>
      assert {:ok, 0} = BPF.run(prog, arp_frame)
    end
  end
end
