# Contributing to picsure

Welcome. This document is the entry point for both new contributors
onboarding to the codebase and internal maintainers who already know
their way around. If you’re looking for end-user documentation, see
[`README.md`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/README.md)
and the vignettes in
[`vignettes/`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/vignettes/)
instead.

> **Transient note (remove after the v3 rewrite ships).** The v3 rewrite
> is currently in flight on the `query_v3` branch; the v1 adapter on
> `main` is frozen until `query_v3` merges in. Until then, branch from
> and target `query_v3` instead of `main`. Once the merge happens, this
> note and the `query_v3` mentions elsewhere in the dev docs should be
> removed — see
> [`TODO.md`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/TODO.md).

## Where to go next

| Topic | Document |
|----|----|
| Docker dev environment | [`docs/development/docker.md`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/docs/development/docker.md) |
| Package architecture | [`docs/development/architecture.md`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/docs/development/architecture.md) |
| Testing | [`docs/development/testing.md`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/docs/development/testing.md) |
| Releasing | [`docs/development/releasing.md`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/docs/development/releasing.md) |

## Local development setup

`picsure` requires R \>= 4.1. The recommended path is the Docker dev
environment (see
[`docs/development/docker.md`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/docs/development/docker.md)),
which preinstalls every system and R dependency. If you’d rather develop
natively:

``` r

# install dev dependencies (pick one)
devtools::install_dev_deps()
# or
pak::pak("local::.")
```

On first call into the package, `reticulate` provisions the upstream
Python `picsure` package into an isolated `uv`-backed virtualenv. This
takes a few seconds the first time only and reuses the cached env on
subsequent calls.

### The iteration loop

``` r

devtools::load_all()       # iterative development; picks up edits to R/ instantly
devtools::test()           # run the full unit suite
devtools::document()       # regenerate man/ from roxygen2 comments
```

What CI runs (reproduce locally before opening a PR):

``` bash
R CMD check .
```

## Branching and pull requests

Branch from `main` for new work and open a PR against `main`. (See the
transient note at the top while the v3 rewrite is in flight.)

- Keep commit messages descriptive — explain why, not just what.
- PRs must pass the `R CMD check` matrix in CI before merge (see
  [`.github/workflows/check.yml`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/.github/workflows/check.yml)).
- If you change exported function signatures, run `devtools::document()`
  and commit the regenerated `man/` and `NAMESPACE` files.

## Code style

- Every exported function is documented with roxygen2. Do not edit
  `man/*.Rd` directly; edit the source and rerun `devtools::document()`.
- The public API uses **camelCase** (`buildClause`, `runQuery`,
  `addFacet`) to match the upstream Python wrapper 1:1. Keep new public
  functions in the same style.
- Internal helpers use snake_case (`to_py_enum`, `with_picsure_error`).
- The package does not configure `styler` or `lintr` — there is no
  enforced formatter. Match the surrounding code.

## Filing issues and asking questions

Use the [GitHub issue
tracker](https://github.com/hms-dbmi/pic-sure-r-adapter-hpds/issues) for
bug reports, feature requests, and questions. Include the output of
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) and a
minimal reproducer when reporting bugs.
