### Added

- Support for bare underscore patterns (`fn _ -> true end`) to accept or reject all packets
- Add expublish for release management

### Fixed

- Remove unreachable final reject instruction when last clause cannot fail

### Changed

- Remove publish-to-hex step from CI (publishing done locally via expublish)
