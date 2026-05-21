# Testing

The test suite has two tiers: a fast unit tier that mocks the
reticulate boundary, and an integration tier that runs against a live
PIC-SURE backend. The unit tier runs on every PR; the integration
tier runs nightly and on demand.

## Layout

| Path                                                            | What it holds                                                                                                    |
|-----------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|
| [`tests/testthat/`](../../tests/testthat/)                      | Unit tests. One `test-<module>.R` per `R/<module>.R`.                                                            |
| [`tests/testthat/helper-mocks.R`](../../tests/testthat/helper-mocks.R) | Fakes for the Python `picsure` module and the `Session` and `FacetSet` it returns. Loaded automatically by testthat. |
| [`tests/testthat/integration/`](../../tests/testthat/integration/) | Live tests (`test-*-live.R`) that talk to a real PIC-SURE instance. Skipped unless `PICSURE_INTEGRATION=1`.    |
| [`tests/testthat.R`](../../tests/testthat.R)                    | Entry point for `R CMD check`.                                                                                   |

## testthat conventions

- `testthat` edition 3 (set in [`DESCRIPTION`](../../DESCRIPTION)
  `Config/testthat/edition: 3`).
- One test file per `R/` source file, named `test-<module>.R`.
  Add new tests to the matching file.
- Snapshot tests are not currently used.

## Mocking the Python boundary

The package-private binding `picsure_py` (declared in
[`R/zzz.R`](../../R/zzz.R)) is the single seam through which every
wrapper reaches Python. Unit tests swap it with a fake via
`testthat::local_mocked_bindings()`.

[`tests/testthat/helper-mocks.R`](../../tests/testthat/helper-mocks.R)
provides:

- `fake_picsure_py(platform_names = ...)` — a list standing in for the
  Python module. Exposes `connect`, `createSubQuery`, `buildQuery`,
  and named lists for `PhenotypicFilterType`, `GroupOperator`, `QueryType`, and
  `Platform`. Records every call on `$.calls`.
- `new_fake_session(...)` — returned by `fake_picsure_py()$connect()`.
  Records calls to `searchDictionary`, `runQuery`, `exportAsPFB`,
  `exportCSV`, `exportTSV`, `loadQueryByID`, `runQueryByID`. Returns
  realistic shapes (a count-result list, a participant `data.frame`,
  etc.) keyed off the `type` argument.
- `new_fake_facet_set()` — a mutable stand-in for the Python
  `FacetSet`. Tracks `$add(key, value)` / `$remove(key, value)` in
  internal state.

A representative pattern from
[`tests/testthat/test-query.R`](../../tests/testthat/test-query.R):

```r
test_that("runQuery() forwards query and type to session$runQuery", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  picsure::runQuery(bdc, q, type = "participant")

  call <- bdc$.calls$runQuery[[1]]
  expect_identical(call$query, q)
  expect_equal(tolower(call$type), "participant")
})
```

To simulate a Python exception (so you can verify
`with_picsure_error()` re-raises it as a `picsureError`), throw a
condition whose class includes `"python.builtin.Exception"`:

```r
bdc$runQuery <- function(...) {
  stop(structure(
    list(message = "query rejected: no SELECT clauses"),
    class = c("python.builtin.Exception", "error", "condition")
  ))
}
```

## Running locally

```r
devtools::test()                                              # full unit suite
devtools::test_active_file("tests/testthat/test-query.R")     # one file
testthat::test_file("tests/testthat/test-query.R", filter = "specific-test")
```

What CI runs (reproduce locally before opening a PR):

```bash
R CMD check .
```

To run tests inside the Docker dev container, see the
[Docker iteration loop](docker.md#the-iteration-loop).

## Integration tests

The integration tier lives under
[`tests/testthat/integration/`](../../tests/testthat/integration/) and
is gated by environment variables. The relevant helpers are in
[`tests/testthat/integration/helper-integration.R`](../../tests/testthat/integration/helper-integration.R).

| Variable                     | What it does                                                                                                                                            |
|------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|
| `PICSURE_INTEGRATION`        | Must be `"1"` for any live test to run. Otherwise every test calls `skip_unless_integration()` and is skipped.                                          |
| `PICSURE_TEST_PLATFORM`      | Platform string or enum-member name (e.g. `BDC_OPEN`). Enum-member names are preferred — they disambiguate platforms that share a human-readable label. |
| `PICSURE_TEST_TOKEN`         | Personal access token. Tests skip if unset.                                                                                                             |
| `PICSURE_TEST_REQUIRE_PATH`  | A concept path used by REQUIRE/SELECT-driven export tests. Tests skip if unset.                                                                         |
| `PICSURE_TEST_SEARCH_TERM`   | Search term for live search tests. Defaults to `"age"`.                                                                                                 |
| `PICSURE_TEST_FACET_CATEGORY`, `PICSURE_TEST_FACET_VALUE` | Facet category and value for live facet tests. Skips if value is unset.                                                       |

Run the integration tier locally (against your own backend):

```bash
export PICSURE_INTEGRATION=1
export PICSURE_TEST_PLATFORM=BDC_OPEN     # or another enum member name
export PICSURE_TEST_TOKEN=...
export PICSURE_TEST_REQUIRE_PATH='\\phs...\\...'
Rscript -e 'testthat::test_local()'
```

The session is cached across tests in a single process (see
`.live_session_cache` in `helper-integration.R`); connecting is
expensive and the live tier is read-only.

## CI

Three workflows under [`.github/workflows/`](../../.github/workflows/):

- [`check.yml`](../../.github/workflows/check.yml) — `R CMD check`
  matrix: R 4.1, 4.3, 4.4 on `ubuntu-latest`, plus R 4.4 on
  `macos-latest`. Runs on PRs and pushes to `main`.
  After the check it runs `covr::package_coverage` and enforces an
  **80% coverage floor** (with `R/zzz.R` excluded). `R/zzz.R` is
  exercised by `library()` itself rather than by tests.
- [`integration.yml`](../../.github/workflows/integration.yml) —
  nightly cron (`13 6 * * *`) plus manual `workflow_dispatch`. Sets
  `PICSURE_INTEGRATION=1` and the secrets `PICSURE_TEST_PLATFORM`,
  `PICSURE_TEST_TOKEN`, `PICSURE_TEST_REQUIRE_PATH`, then runs
  `testthat::test_local()`. On a scheduled failure it opens an issue
  tagged `integration-failure`.
- [`pkgdown.yml`](../../.github/workflows/pkgdown.yml) — builds the
  pkgdown site on PR (no deploy), push, release publish, and
  `workflow_dispatch`. Deploys to the `gh-pages` branch on non-PR
  events.

## Debugging a CI failure locally

The check matrix uses pinned R minor versions. To reproduce a failing
matrix entry, use the Docker dev environment with the same R version:
edit the `FROM` line in `docker/Dockerfile` to the matching
`rocker/rstudio:<version>` and rebuild, then run `R CMD check .`
inside the `dev` service. See
[`docker.md#bumping-the-r-version`](docker.md#bumping-the-r-version).

## The enum parity invariant

[`tests/testthat/test-enums-parity.R`](../../tests/testthat/test-enums-parity.R)
guards the rule that R-side enums in
[`R/enums.R`](../../R/enums.R) must exactly mirror the Python
`picsure` enums. It runs only when `picsure_py` is a real Python
module (it skips when the testthat fake is bound), so it executes
under integration runs and on any local invocation after Python has
been initialized.

The test checks:

1. For each of `PhenotypicFilterType`, `GroupOperator`, `QueryType`,
   `Platform`: R member names match Python member names, and each
   member's `$name` and `$value` match.
2. For `Platform`: every dataclass field on the Python
   `PlatformConfig` (`url`, `resource_uuid`, `label`,
   `include_consents`, `requires_auth`) is mirrored on the R member.
3. The set of public `Enum` subclasses exported by the Python package
   exactly equals `c("PhenotypicFilterType", "GroupOperator", "Platform",
   "QueryType")`. **If you add a new enum in Python, this test fails
   until you mirror it in `R/enums.R` and add it to this list.**
