# Changelog

## [Unreleased]

### Changed

- Make `Additional::Text::path` [`camino::Utf8PathBuf`](https://docs.rs/camino/1/camino/struct.Utf8PathBuf.html)
- Removed `default_match: Match` field from `RetrieveTestCases`.
- Added `note` field to `Verdict::WrongAnswer`.

## [0.12.0] - 2021-03-01Z

### Changed

- Added `{DeterministicExpectedoutput, Match}::SplitWhitespace`.
- Added `default_match: Match` field to `RetrieveTestCases`.

## [0.11.1] - 2021-02-26Z

### Fixed

- Fixed around <kbd>Ctrl-c</kbd> during `judge`.

## [0.11.0] - 2021-02-26Z

### Changed

- Made `Additional::SystemTestCases::problem` `Option<Url>`.

## [0.10.0] - 2021-02-25Z

### Changed

- Added `Additional::SystemTestCases`.
- Added an argument to `BatchTestSuite::load_test_cases`.

## [0.9.2] - 2021-02-23Z

### Fixed

- Fixed [a problem where processes are not killed for timeout](https://github.com/qryxip/cargo-compete/issues/135).

## [0.9.1] - 2021-02-18Z

### Fixed

- Added `#[serde(default)]` to `PartialBatchTestCase::out: Option<Arc<str>>`.

    Previously, explicit `out: ~` had been allowed but the field itself was required.

## [0.9.0] - 2021-02-15Z

### Changed

- Added `Checker` variant to `Match`.

    ```yaml
    match:
      Checker:
        cmd: cat "$ACTUAL_OUTPUT" | cargo run --bin check-a
        shell: Bash
    ```

    ```yaml
    match:
      Checker:
        cmd: ~/.cache/online-judge-tools/library-checker-problems/math/sqrt_mod/checker "$INPUT" "$ACTUAL_OUTPUT" "$EXPECTED_OUTPUT"
        shell: Bash
    ```

## [0.8.2] - 2021-02-14Z

### Fixed

- Added a workaround for large process input/output. ([#121](https://github.com/qryxip/snowchains/pull/121))

- Fixed a problem where string values in YAMLs are unnecessarily quoted. ([#121](https://github.com/qryxip/snowchains/pull/121))

    This problem was caused by [a change](https://github.com/dtolnay/serde-yaml/commit/ef990758a19d4d845cf19a8943e7d905909cafd8) in `serde-yaml v0.8.16`, which was released in February 2, 2021.

## [0.8.1] - 2021-01-21Z

### Changed

- Improved around Dropbox.

### Fixed

- Fixed URL parsing for Codeforces.

## [0.8.0] - 2021-01-17Z

### Changed

- Updated `reqwest` to v0.11.
- Updated `tokio` to v1.

## [0.7.0] - 2020-12-13Z

### Changed

- Replaced `{Atcoder, Codeforces}{RetrieveTestCasesTargets, SubmitTarget}` with `{ProblemsInContest, ProblemInContest}`. ([#110](https://github.com/qryxip/snowchains/pull/110))

    You can use URLs for `ProblemsInContest`, `ProblemInContest`.

- Modified `RetrieveTestCasesOutcome`. ([#110](https://github.com/qryxip/snowchains/pull/110))

## [0.6.0] - 2020-11-24Z

### Fixed

- Stopped asking username and password when you have already logged in AtCoder. ([#106](https://github.com/qryxip/snowchains/pull/106))

### Removed

- Dropped support for `x86_64-unknown-linux-musl`. ([#105](https://github.com/qryxip/snowchains/pull/105))

## [0.5.2] - 2020-10-18Z

### Fixed

- `RetrieveTestCasesOutcomeProblem::index`es for yukicoder contests will be alphabets. ([#102](https://github.com/qryxip/snowchains/pull/102))
    Previously, "problem no"s were set.

## [0.5.1] - 2020-09-25Z

### Fixed

- Supports "小数誤差許容問題" in yukicoder. ([#96](https://github.com/qryxip/snowchains/pull/96))

## [0.5.0] - 2020-08-25Z

### Added

- Enabled downloading all of the test cases on Dropbox.

### Changed

- Added `url` and `display_name` fields to `RetrieveTestCasesOutcomeContest`.
- Made `Outcome`s `#[non_exhaustive]`.

## [0.4.3] - 2020-08-23Z

### Fixed

- Fixed a cosmetic problem on `WatchSubmissions`.

## [0.4.2] - 2020-08-20Z

### Changed

- Now `RetrieveTestCases` for AtCoder proceeds when encountered scraping errors.

### Fixed

- Improved the parser for `RetrieveTestCases`.

## [0.4.1] - 2020-08-18Z

### Fixed

- Improved `RetrieveTestCases` for AtCoder. ([#76](https://github.com/qryxip/snowchains/pull/76))

## [0.4.0] - 2020-08-16Z

### Changed

- Made `RetrieveTestCasesOutcomeProblem.screen_name` `Option<String>`.
- Made `SubmitOutcome.problem_screen_name` `Option<String>`.

## [0.3.1] - 2020-08-07Z

### Fixed

- Fixed the parser for AtCoder submissions. ([#71](https://github.com/qryxip/snowchains/pull/71))

## [0.3.0] - 2020-08-06Z

### Added

- Added `RetrieveSubmissionSummaries` action for AtCoder.

## [0.2.0] - 2020-08-05Z

### Changed

- Added `names` argument to `BatchTestSuite::load_test_cases`.
