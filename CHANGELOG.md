## [Unreleased]

### Added

* `--mode` option to `judge`, `restore`, and `submit`
* `--release` option to `judge`, which equivalent to `--mode release`
* `--debug` option to `submit`, which equivalent to `--mode debug`

### Changed

* Make `env` field more configurable

### Removed

* `show _` commands
* `modify _` commands

## [0.2.0] - 2019-03-03

### Added

* Codeforces support
* `list-langs` command
* `session.robots` field (default: `true`)

### Changed

* `session.api_tokens` field (required)
* `languages._.language_ids` to `languages._.names` (always required)

### Fixed

* Suppressed the unnecessary newline when printing errors
