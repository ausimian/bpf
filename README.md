# BPF

[![CI](https://github.com/ausimian/bpf/actions/workflows/ci.yml/badge.svg)](https://github.com/ausimian/bpf/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/ausimian/bpf/badge.svg?branch=main)](https://coveralls.io/github/ausimian/bpf?branch=main)
[![Hex.pm](https://img.shields.io/hexpm/v/bpf.svg)](https://hex.pm/packages/bpf)
[![Hex Docs](https://img.shields.io/badge/hex-docs-blue.svg)](https://hexdocs.pm/bpf)

> **Note:** This library is a work in progress and not yet suitable for production use.

A library for compiling Elixir binary pattern matching expressions into classic BPF (Berkeley Packet Filter) bytecode.

Write packet filters using familiar Elixir syntax and compile them to BPF programs that can be attached to sockets or used with `libpcap`.

## Features

- **Elixir syntax** - Write filters using binary pattern matching and guards
- **Multi-clause support** - Multiple patterns with fallthrough semantics
- **Guard expressions** - Comparisons, logical operators, bitwise operations, arithmetic
- **Packet length filtering** - Use `byte_size(packet)` to filter by packet size
- **SSA-based compiler** - Optimized code generation with register allocation

## Usage

```elixir
# Filter IPv4 packets with IHL >= 5
prog = BPF.compile(fn <<4::4, ihl::4, _::binary>> when ihl >= 5 -> true end)

# Test against a packet
BPF.interpret(prog, <<0x45, 0x00, 0x00, 0x28>>)
#=> true

# Get raw bytecode for use with SO_ATTACH_FILTER
bytes = BPF.assemble(prog)
```

### Multi-clause Filters

```elixir
# Accept IPv4 or IPv6 packets over 100 bytes
prog = BPF.compile(fn
  <<4::4, _ihl::4, _tos::8, len::16, _::binary>> when len > 100 -> true
  <<6::4, _::28, len::16, _::binary>> when len + 40 > 100 -> true
end)
```

### Packet Length Filtering

Bind the full packet with `= packet` to use `byte_size/1`:

```elixir
# Accept packets of at least 64 bytes
prog = BPF.compile(fn <<_::binary>> = packet when byte_size(packet) >= 64 -> true end)

# Combine with pattern matching
prog = BPF.compile(fn <<4::4, _::4, _::binary>> = pkt when byte_size(pkt) >= 20 -> true end)
```

### Conditional Returns

Return a boolean expression to accept/reject based on runtime values:

```elixir
# Accept IPv4 or IPv6 packets
prog = BPF.compile(fn <<version::4, _::binary>> -> version == 4 or version == 6 end)
```

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
- Packet length: `byte_size(packet)` when bound with `= packet`

## Installation

Add `bpf` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:bpf, "~> 0.1.0"}
  ]
end
```

