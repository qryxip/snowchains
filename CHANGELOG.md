## [Unreleased]

### Added

* `--mode` option to `judge`, `restore`, and `submit`
* `--release` option to `judge`, which equivalent to `--mode release`
* `--debug` option to `submit`, which equivalent to `--mode debug`
* Add `.command_line_arguments` to `$SNOWCHAINS_RESULT` s.

### Changed

* Make `env` field more configurable
* Now it prints most of the information to stderr, not stdout
* Move `service`, `contest` and `language` fields of `snowchains.toml` to another file

### Removed

* `show _` commands
* `modify _` commands

### Fixed

* Fix the condition of enabling ANSI color when `--color auto`
* Fix a bug where `list-langs` displays &lt;number of problems&gt; copies of a list
* Prevent `list-langs` from displaying unavailable languages when `problem` is specified
* Change the default value of `jobs` from `1` to the number of CPUs

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
