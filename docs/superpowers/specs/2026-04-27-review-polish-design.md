# Design Spec — `query_v3` review-polish pass

**Date:** 2026-04-27
**Branch base:** `query_v3` (post-remediation, all 6 Blockers resolved)
**Branch:** `query_v3_polish`
**Source review:** `docs/superpowers/reviews/2026-04-21-query-v3-review.md`

## Goal

Resolve five Important review findings (I1, I5, I6, I7, I9) and two cheap post-merge follow-ups (P2, P5) in a single stacked branch off `query_v3`. After this pass, `query_v3` is ready to merge to `main` once the Python `picsure` package and backend URLs are published. Out of scope and deferred to GitHub issues: P1 (macOS R-version matrix), P3 (`showAllFacets` exposure), P4 (end-to-end smoke test).

## Scope

| ID | Source | Title | Treatment |
|----|--------|-------|-----------|
| I7 | R5 (Important) | Constructor/handler naming split | Rename constructor to `picsureError()` |
| I1 | R3 (Important) | Error-message extraction divergence vs. spec | Verify, then no-op or align |
| I5 | R1 (Important) | `connect(...)` undocumented kwargs | Whitelist + document, snake_case params |
| I6 | R4 (Important) | Missing wrapper tests | Add 2 tests |
| I9 | R2 (Important) | `picsure==0.1.0` pin will break silently | Add explanatory comment |
| P2 | R6 (Follow-up) | Coverage computed twice in `check.yml` | Consolidate to one `covr::package_coverage()` call |
| P5 | R6 (Follow-up) | `devtools` in Suggests pulls ~30 deps | Drop; switch CI to `testthat::test_dir()` |

## Section 1 — Constructor rename (I7)

The exported error constructor is `picsure_error()` (snake_case) while the class is `picsureError` (camelCase). A user copying the constructor name into a `tryCatch` handler — `tryCatch(connect(...), picsure_error = handler)` — silently misses every error because the actual class is `picsureError`. The constructor is the only non-camelCase exported symbol in the package; the API preference is camelCase.

**Change.** Rename the constructor `picsure_error` → `picsureError`. Producer and handler now share the same identifier.

**Touched.**
- `R/errors.R` — function definition, `@export` tag, all internal callers (currently only `with_picsure_error`).
- `NAMESPACE` — regenerate via roxygen.
- `man/picsure_error.Rd` → `man/picsureError.Rd` — regenerate.
- `tests/testthat/test-errors.R` — update assertions.
- `vignettes/running-and-exporting.Rmd` — already references the class as `picsureError`; verify constructor examples (if any) match.
- `_pkgdown.yml` — reference index entry.
- Any other `picsure_error(` call sites across `R/` (grep confirms none beyond `with_picsure_error`).

**No alias, no deprecation shim.** Package is unreleased; no install base.

## Section 2 — `with_picsure_error` verification (I1)

The design spec says extract Python error messages via `reticulate::py_last_error()$message`. The shipped code uses `conditionMessage(e)` directly on the captured `python.builtin.Exception` condition. Reviewer R3 flagged this as a possible production divergence but did not verify against a live Python exception.

The integration suite currently passes against the live backend with the existing code, suggesting `conditionMessage(e)` already extracts the Python exception's `str()` — that's reticulate's standard translation for `python.builtin.Exception` conditions. The local-condition read is also preferable on principle: `py_last_error()` reads global Python error state and could be cleared or clobbered between the exception and our read.

**Change.** Two-step.

1. **Probe.** Add `tests/testthat/integration/test-error-extraction.R` that triggers a real `PicSureError` (e.g. `connect(platform = "BDC Authorized", token = "definitely-invalid")`) and captures the resulting condition. Assert both `conditionMessage(e)` and `reticulate::py_last_error()$message` produce the same non-empty string.

2. **Branch on result.**
   - **Equal (expected).** Keep `conditionMessage(e)`. The probe stays as a regression test locking the contract. Update `docs/superpowers/specs/2026-04-20-r-adapter-rewrite-design.md` to replace its `py_last_error()$message` reference with `conditionMessage(e)` + a one-sentence note on why the local-condition read is preferable.
   - **Diverge.** Update `with_picsure_error` to `reticulate::py_last_error()$message %||% conditionMessage(e)` (fallback to local read if global state is empty). The probe becomes the assertion.

The probe runs under the existing integration tier (`PICSURE_INTEGRATION=1` + VPN + token). If credentials are unavailable it self-skips via `skip_unless_integration()`.

## Section 3 — `connect(...)` whitelist + docs (I5)

`connect(platform, token, ...)` forwards `resource_uuid`, `include_consents`, `requires_auth` to Python through `...` with no validation or documentation. A typo — say `resourceUuid` (camelCase) — silently no-ops because Python's `connect()` accepts `**kwargs` and discards unknown ones.

**Change.** Validate and document.

```r
# R/connect.R
CONNECT_EXTRA_KWARGS <- c("resource_uuid", "include_consents", "requires_auth")

connect <- function(platform, token, ...) {
  extras <- list(...)
  unknown <- setdiff(names(extras), CONNECT_EXTRA_KWARGS)
  if (length(unknown) > 0) {
    stop(picsureError(sprintf(
      "Unknown argument(s): %s. Valid extras: %s.",
      paste(sprintf("`%s`", unknown), collapse = ", "),
      paste(sprintf("`%s`", CONNECT_EXTRA_KWARGS), collapse = ", ")
    )))
  }
  # ... existing body
}
```

Roxygen gains three `@param` entries (snake_case names matching Python kwargs, descriptions sourced from the Python adapter's docstring). The `...` line in roxygen is replaced with explicit `@param` entries plus a note that other kwargs are rejected.

**Naming decision.** Parameter names stay snake_case to match Python kwargs 1:1. The package's camelCase preference applies to *function names* (`runQuery`, `addFacet`); reticulate-boundary parameter names are noisier to translate than to mirror.

**Tests.** In `tests/testthat/test-connect.R`:
- Unknown kwarg (`resourceUuid = "x"`) → `expect_error(..., class = "picsureError")` whose message names the three valid keys.
- All three valid kwargs forward through to the fake `connect`.

## Section 4 — Mechanical fixes (I6, I9, P2, P5)

### I6 — missing tests

Two `test_that` blocks added to `tests/testthat/test-export.R`:
- `exportCSV(session, df)` without `path` → `expect_error` with `fixed = TRUE` matching the wrapper's missing-arg message.
- `exportTSV(session, "not a data frame", tempfile())` → `expect_error` whose message names `data.frame` and references `runQuery(..., type = "participant")` (matches the existing `exportCSV` non-data.frame test established in remediation T6).

### I9 — version pin comment

Above the `reticulate::py_require("picsure==0.1.0")` line in `R/zzz.R`, add:

```r
# Pin is intentional. The Python `picsure` package is pre-1.0 and reserves the
# right to break behavior across patch releases. Bumping requires re-running
# the integration suite under VPN against a backend that ships the new server
# protocol. See follow-up: relax to a compatible range once `picsure` reaches
# 1.0 (tracked in the project README).
```

No code change.

### P2 — covr deduplication

`.github/workflows/check.yml` currently computes `covr::package_coverage()` once for the report (line 51) and again for the floor check (line 57). Consolidate into one Rscript invocation:

```yaml
- name: Run coverage and enforce 80% floor
  run: |
    Rscript -e '
      cov <- covr::package_coverage(type = "tests", line_exclusions = list("R/zzz.R"))
      writeLines(capture.output(print(cov)), "coverage.txt")
      pct <- covr::percent_coverage(cov)
      cat(sprintf("Coverage: %.1f%%\n", pct))
      if (pct < 80) stop(sprintf("Coverage %.1f%% below 80%% floor", pct))
    '
```

Halves CI time on the coverage step.

### P5 — drop `devtools`

- `DESCRIPTION` line 23: remove `devtools,` from Suggests.
- `DESCRIPTION` Suggests: add `pkgload` (a much lighter dep than `devtools`; `testthat::test_local()` requires it to load the source package).
- `.github/workflows/integration.yml` line 34: change `Rscript -e "devtools::test()"` → `Rscript -e "testthat::test_local()"`. `test_local()` reads `DESCRIPTION`, loads the package via `pkgload`, and runs `tests/testthat` — the closest drop-in replacement for `devtools::test()`.

`devtools` pulls ~30 transitive dependencies. `pkgload` brings in 5–6. Net win on dep graph size and CI install time.

## Implementation order

The 7 items are largely independent; the rename in Section 1 should land first because subsequent code/test changes will reference the new constructor name.

1. **I7** — constructor rename. Touches the most files; everything else builds on it.
2. **I5** — `connect(...)` whitelist + docs.
3. **I6** — missing export tests.
4. **I9** — version pin comment.
5. **P2** — covr consolidation.
6. **P5** — drop devtools, switch CI test invocation.
7. **I1** — error-extraction probe (requires VPN to actually run; the test code itself can land before VPN run).

One commit per logical unit. No co-author trailers (per project preference).

## Verification

After each commit: `Rscript -e "devtools::load_all(); testthat::test_dir('tests/testthat')"` (still uses devtools locally; CI changes are isolated to the integration workflow).

After all commits land:
- `R CMD check` — expect 0 errors, 0 warnings, ≤1 transient NOTE (matches current state).
- Unit tier — expect 153+ passing (we add ~3 unit tests; total floor ~156).
- Integration tier — under VPN + secrets, expect previous count + 1 (the new error-extraction probe).
- `_pkgdown.yml` reference resolves without warnings (the renamed `picsureError` constructor must appear).

## Out of scope (deferred to GitHub issues post-merge)

- **P1** — macOS matrix gap (R 4.4 only). Discuss when we set the release matrix.
- **P3** — `showAllFacets()` exposure. New exported function; needs API review.
- **P4** — End-to-end smoke test. Better as a real integration scenario than a faked sequence.

## Risks

- **I7 rename ripple.** Internal callers are limited but missing one would break `R CMD check`. Mitigation: grep for `picsure_error\(` after the rename; CI catches anything missed.
- **I1 verification needs VPN.** The probe self-skips without credentials; the spec/code change is gated on a successful run.
- **P5 CI change.** `testthat::test_local()` requires `pkgload`. Mitigation: add `pkgload` to Suggests (covered above) and verify on first CI run after the polish branch lands.
