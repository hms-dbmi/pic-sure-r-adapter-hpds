# Contributing to `picsure`

Welcome. This document is the entry point for both new contributors
onboarding to the codebase and internal maintainers who already know
their way around. If you're looking for end-user documentation, see
[`README.md`](README.md) and the vignettes in
[`vignettes/`](vignettes/) instead.

> **Note on branches.** The 2.0.0 rewrite lives on the `query_v3`
> branch and is released as the `v2.0.0` tag. The legacy 1.x adapter
> remains on `main` during the migration window, so the two coexist;
> `query_v3` is **not** being merged into `main` for now. Branch from
> and target `query_v3` for all new work. See [`TODO.md`](TODO.md) for
> the eventual `main` cutover.

## Where to go next

| Topic                        | Document                                                       |
|------------------------------|----------------------------------------------------------------|
| Docker dev environment       | [`docs/development/docker.md`](docs/development/docker.md)     |
| Package architecture         | [`docs/development/architecture.md`](docs/development/architecture.md) |
| Testing                      | [`docs/development/testing.md`](docs/development/testing.md)   |
| Releasing                    | [`docs/development/releasing.md`](docs/development/releasing.md) |

## Local development setup

`picsure` requires R >= 4.1. The recommended path is the Docker dev
environment (see [`docs/development/docker.md`](docs/development/docker.md)),
which preinstalls every system and R dependency. If you'd rather
develop natively:

```r
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

```r
devtools::load_all()       # iterative development; picks up edits to R/ instantly
devtools::test()           # run the full unit suite
devtools::document()       # regenerate man/ from roxygen2 comments
```

What CI runs (reproduce locally before opening a PR):

```bash
R CMD check .
```

## Branching and pull requests

Branch from `query_v3` for new work and open a PR against `query_v3`
(see the note on branches at the top). The legacy 1.x adapter on `main`
is not under active development.

- Keep commit messages descriptive — explain why, not just what.
- PRs must pass the `R CMD check` matrix in CI before merge
  (see [`.github/workflows/check.yml`](.github/workflows/check.yml)).
- If you change exported function signatures, run `devtools::document()`
  and commit the regenerated `man/` and `NAMESPACE` files.

## Code style

- Every exported function is documented with roxygen2. Do not edit
  `man/*.Rd` directly; edit the source and rerun `devtools::document()`.
- The public API uses **camelCase** (`buildClause`, `runQuery`,
  `addFacet`) to match the upstream Python wrapper 1:1. Keep new
  public functions in the same style.
- Internal helpers use snake_case (`to_py_enum`, `with_picsure_error`).
- The package does not configure `styler` or `lintr` — there is no
  enforced formatter. Match the surrounding code.

## Filing issues and asking questions

Use the [GitHub issue tracker](https://github.com/hms-dbmi/pic-sure-r-adapter-hpds/issues)
for bug reports, feature requests, and questions. Include the output
of `sessionInfo()` and a minimal reproducer when reporting bugs.
