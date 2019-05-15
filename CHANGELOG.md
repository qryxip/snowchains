## [Unreleased]

### Added

* `--mode` option to `judge`, `retrieve submissions`, and `submit`.
* `--release` option to `judge`, which equivalent to `--mode release`.
* `--debug` option to `submit`, which equivalent to `--mode debug`.
* Add `.command_line_arguments`, `.config`, `.base_directory`, and `.target` to `$SNOWCHAINS_RESULT` s.
* Add `--output` option and `--json` flag to all of the subcommands.
* Add `--fetch-all` flag to `retrieve submissions` command.

### Changed

* Rename `download` command to `retrive testcases` (`download` remains as an alias)
* Rename `restore` command to `retrive submissions`
* Rename `list-langs` command to `retrive languages`
* Make `env` field more configurable.
* Now it prints most of the information to stderr, not stdout.
* Move `service`, `contest` and `language` fields of `snowchains.toml` to another file.
* Make it find a executable binary's path with [`which`](https://crates.io/crates/which) crate.
* Now it retrieves and parses `tasks_print` pages to improve the performance.
* Save cookies as JSON files.

### Removed

* `show _` commands.
* `modify _` commands.

### Fixed

* Fix the condition of enabling ANSI color when `--color auto`.
* Fix a bug where `retrive languages` displays &lt;number of problems&gt; copies of a list.
* Prevent `retrieve languages` from displaying unavailable languages when `problem` is specified.
* Change the default value of `jobs` from `1` to the number of CPUs.

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
