# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Support for bare underscore patterns (`fn _ -> true end`) to accept or reject all packets

## [0.1.1] - 2025-01-05

### Added

- Initial public release
- Compile Elixir binary pattern matching to classic BPF bytecode
- Support for literals, bindings, and skip segments in patterns
- Support for size modifiers (big/little endian, signed/unsigned)
- Guard expressions with comparisons, logical operators, and arithmetic
- Bitwise operations (`band`, `bor`, `bxor`, `bnot`)
- `byte_size/1` support for packet length checks
- Multi-clause function support
- BPF interpreter for testing
- BPF assembler for generating raw bytecode
