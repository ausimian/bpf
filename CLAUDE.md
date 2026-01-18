# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

BPF is an Elixir library that compiles Elixir binary pattern matching expressions into classic BPF (Berkeley Packet Filter) bytecode. It allows writing packet filters using Elixir syntax that can be attached to sockets or used with libpcap.

## Common Commands

```bash
# Run all tests
mix test

# Run a single test file
mix test test/compiler_test.exs

# Run a specific test by line number
mix test test/compiler_test.exs:42

# Run tests with coverage
mix coveralls

# Compile the project
mix compile

# Generate documentation
mix docs
```

## Architecture

The compilation pipeline flows through these stages:

1. **Parser** (`lib/bpf/parser.ex`) - Parses quoted Elixir AST from `fn` expressions into IR (Intermediate Representation). Handles binary pattern segments, guards, and return actions.

2. **IR** (`lib/bpf/ir.ex`) - Defines the intermediate representation structures:
   - `Segment` - Binary pattern segments (literal, binding, skip)
   - `Pattern` - Collection of segments
   - `Guard` - Comparison, logical, bitwise, and arithmetic operations
   - `Clause` - A pattern + guard + action

3. **Compiler** (`lib/bpf/compiler.ex`) - Orchestrates the compilation pipeline, handling multi-clause functions with fallthrough semantics and label resolution.

4. **SSA** (`lib/bpf/compiler/ssa.ex`) - Converts IR clauses to SSA (Static Single Assignment) form with virtual registers. Handles pattern matching as loads/checks and guards as comparisons.

5. **Liveness** (`lib/bpf/compiler/liveness.ex`) - Analyzes which virtual registers are live at each program point.

6. **RegAlloc** (`lib/bpf/compiler/regalloc.ex`) - Allocates virtual registers to BPF's limited registers (A, X) and scratch memory slots.

7. **CodeGen** (`lib/bpf/compiler/codegen.ex`) - Generates actual BPF instruction tuples from SSA operations with register allocation.

8. **Interpreter** (`lib/bpf/interpreter.ex`) - Executes BPF programs against binary packets for testing.

9. **Program/Instruction** (`lib/bpf/program.ex`, `lib/bpf/instruction.ex`) - BPF program struct and instruction encoding for `assemble/1`.

## Key Concepts

- BPF operates on 32-bit unsigned values with two registers (A and X) plus 16 scratch memory slots
- The `BPF.compile/1` macro captures AST at compile time and produces a `%BPF.Program{}` struct
- Multi-clause functions compile to sequential clause blocks with fallthrough on failure
- Sub-byte fields (like `<<version::4>>`) use load + shift + mask operations
