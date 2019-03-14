## [Unreleased]

### Added

* `--mode` option to `judge`, `restore`, and `submit`
* `--release` option to `judge`, which equivalent to `--mode release`
* `--debug` option to `submit`, which equivalent to `--mode debug`

### Changed

* Make `env` field more configurable
* Now it prints most of the information to stderr, not stdout

### Removed

* `show _` commands
* `modify _` commands

### Fixed

* Fix a bug where `list-langs` displays &lt;number of problems&gt; copies of a list
* Prevent `list-langs` from displaying unavailable languages when `problem` is specified

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
