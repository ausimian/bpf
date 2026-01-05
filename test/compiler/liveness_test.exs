defmodule BPF.Compiler.LivenessTest do
  use ExUnit.Case, async: true

  alias BPF.Compiler.Liveness

  describe "analyze/1" do
    test "tracks load_packet definitions" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :eq, 0, {:imm, 42}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      assert %{def: 0, last_use: 1} = liveness[0]
    end

    test "tracks load_imm definitions" do
      ops = [
        {:load_imm, 0, 100},
        {:cmp, :eq, 0, {:imm, 42}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      assert %{def: 0, last_use: 1} = liveness[0]
    end

    test "tracks ALU definitions and uses" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :add, 0, 1},
        {:cmp, :gt, 2, {:imm, 100}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      # x defined at 0, used at 2 (in ALU)
      assert %{def: 0, last_use: 2} = liveness[0]
      # y defined at 1, used at 2 (in ALU)
      assert %{def: 1, last_use: 2} = liveness[1]
      # result defined at 2, used at 3 (in cmp)
      assert %{def: 2, last_use: 3} = liveness[2]
    end

    test "tracks cmp_success uses" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp_success, :eq, 0, {:imm, 42}, :success}
      ]

      liveness = Liveness.analyze(ops)

      assert %{def: 0, last_use: 1} = liveness[0]
    end

    test "tracks ret uses with vreg" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:ret, 0}
      ]

      liveness = Liveness.analyze(ops)

      assert %{def: 0, last_use: 1} = liveness[0]
    end

    test "tracks multiple uses of same vreg" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :gt, 0, {:imm, 5}, :fail},
        {:cmp, :lt, 0, {:imm, 100}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      info = liveness[0]
      assert info.def == 0
      assert info.last_use == 2
      assert length(info.uses) == 2
    end

    test "ignores immediate values in comparisons" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :eq, 0, {:imm, 42}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      # Should only have vreg 0, not the immediate
      assert Map.keys(liveness) == [0]
    end

    test "ignores :accept and :reject in ret" do
      ops = [
        {:ret, :accept}
      ]

      liveness = Liveness.analyze(ops)

      assert liveness == %{}
    end

    test "ignores labels and jumps" do
      ops = [
        {:label, :start},
        {:load_packet, 0, :byte, 0, 0, nil},
        {:jump, :end},
        {:label, :end},
        {:ret, :accept}
      ]

      liveness = Liveness.analyze(ops)

      # Only vreg 0 should be tracked
      assert Map.keys(liveness) == [0]
    end

    test "handles vreg comparisons (both sides)" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :eq, 0, 1, :fail}
      ]

      liveness = Liveness.analyze(ops)

      assert %{def: 0, last_use: 2} = liveness[0]
      assert %{def: 1, last_use: 2} = liveness[1]
    end

    test "handles nil in ALU src2 (unary ops)" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:alu, 1, :bnot, 0, nil},
        {:ret, 1}
      ]

      liveness = Liveness.analyze(ops)

      assert %{def: 0, last_use: 1} = liveness[0]
      assert %{def: 1, last_use: 2} = liveness[1]
    end
  end

  describe "live_range/2" do
    test "returns {start, end} tuple for existing vreg" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :eq, 0, {:imm, 42}, :fail},
        {:ret, :accept}
      ]

      liveness = Liveness.analyze(ops)

      assert {0, 1} = Liveness.live_range(liveness, 0)
    end

    test "returns nil for non-existent vreg" do
      liveness = %{}

      assert Liveness.live_range(liveness, 99) == nil
    end
  end

  describe "overlaps?/3" do
    test "returns true for overlapping ranges" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :add, 0, 1}
      ]

      liveness = Liveness.analyze(ops)

      # vreg 0: [0, 2], vreg 1: [1, 2] - they overlap
      assert Liveness.overlaps?(liveness, 0, 1)
    end

    test "returns false for non-overlapping ranges" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :eq, 0, {:imm, 42}, :fail},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:cmp, :eq, 1, {:imm, 10}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      # vreg 0: [0, 1], vreg 1: [2, 3] - no overlap
      refute Liveness.overlaps?(liveness, 0, 1)
    end

    test "returns false when first vreg doesn't exist" do
      liveness = %{1 => %{def: 0, last_use: 1, uses: [1]}}

      refute Liveness.overlaps?(liveness, 99, 1)
    end

    test "returns false when second vreg doesn't exist" do
      liveness = %{0 => %{def: 0, last_use: 1, uses: [1]}}

      refute Liveness.overlaps?(liveness, 0, 99)
    end

    test "handles edge case where ranges touch" do
      # Range 1: [0, 1], Range 2: [1, 2]
      # At index 1, both are live - this is overlap
      liveness = %{
        0 => %{def: 0, last_use: 1, uses: [1]},
        1 => %{def: 1, last_use: 2, uses: [2]}
      }

      assert Liveness.overlaps?(liveness, 0, 1)
    end
  end

  describe "live_at/2" do
    test "returns all vregs live at given index" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:load_packet, 1, :byte, 1, 0, nil},
        {:alu, 2, :add, 0, 1},
        {:cmp, :gt, 2, {:imm, 100}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      # At index 2: vreg 0 [0,2], vreg 1 [1,2], vreg 2 [2,3] are all live
      live = Liveness.live_at(liveness, 2)
      assert Enum.sort(live) == [0, 1, 2]
    end

    test "returns empty list when no vregs are live" do
      ops = [
        {:load_packet, 0, :byte, 0, 0, nil},
        {:cmp, :eq, 0, {:imm, 42}, :fail}
      ]

      liveness = Liveness.analyze(ops)

      # At index 5, nothing is live (past all uses)
      assert Liveness.live_at(liveness, 5) == []
    end

    test "handles index before any definitions" do
      liveness = %{
        0 => %{def: 2, last_use: 3, uses: [3]}
      }

      assert Liveness.live_at(liveness, 0) == []
    end

    test "includes vreg at its definition point" do
      liveness = %{
        0 => %{def: 1, last_use: 3, uses: [3]}
      }

      assert Liveness.live_at(liveness, 1) == [0]
    end

    test "includes vreg at its last use point" do
      liveness = %{
        0 => %{def: 1, last_use: 3, uses: [3]}
      }

      assert Liveness.live_at(liveness, 3) == [0]
    end
  end
end
