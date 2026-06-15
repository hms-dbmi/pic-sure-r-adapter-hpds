# Releasing

This document is for maintainers cutting a new release of `picsure`.

## Versioning

`picsure` follows [SemVer](https://semver.org/). The version lives in
the [`DESCRIPTION`](../../DESCRIPTION) `Version:` field. The current
value is `2.0.0` — the first full release of the rewritten adapter.
By R packaging convention, a `*.9000` suffix marks an in-progress
development track on top of the last released version; after releasing
`2.0.0` you'd bump development to `2.0.0.9000` until the next release.

The release tag is `v2.0.0` (cut from the `query_v3` branch); the only
prior tag was `v1.0.0-alpha`. Confirm the next release tag with
maintainers before cutting.

## Pre-release checklist

1. **Changelog.** Update [`NEWS.md`](../../NEWS.md) at the package
   root — add a `# picsure X.Y.Z` section describing the release.
   pkgdown wires it into the navbar automatically.
2. **Bump `DESCRIPTION` `Version:`** to the release version (drop
   the `.9000` development suffix).
3. **Documentation is current.** Run `devtools::document()` and
   verify `man/` and `NAMESPACE` have no uncommitted changes.
4. **Tests pass on the full matrix.** Push to a release branch and
   confirm `check.yml` is green across R 4.1 / 4.3 / 4.4 on Ubuntu
   and R 4.4 on macOS.
5. **Integration tier passes.** Trigger
   [`integration.yml`](../../.github/workflows/integration.yml) via
   `workflow_dispatch` and confirm green.
6. **`R CMD check --as-cran .` is clean** locally:
   ```bash
   R CMD check --as-cran .
   ```
   The CI matrix already passes `--as-cran` (see
   [`check.yml`](../../.github/workflows/check.yml)), so this should
   be a confirmation step.
7. **Vignettes knit.** `devtools::build_vignettes()` succeeds.
8. **pkgdown site builds.** `pkgdown::build_site()` succeeds locally
   (or rely on the `pkgdown.yml` workflow's PR build).

## Cutting the release

The current release tag is `v2.0.0` (cut from `query_v3`); the only
prior tag was `v1.0.0-alpha`. New tags follow `vX.Y.Z`. Confirm the
convention with maintainers before cutting.

```bash
# from a clean working tree on the release commit
git tag -a vX.Y.Z -m "picsure vX.Y.Z"
git push origin vX.Y.Z
```

Pushing the tag alone does **not** create a GitHub Release — that is
a separate step. After pushing the tag, publish a Release for it
either through the GitHub UI (**Releases → Draft a new release →
choose tag**) or with `gh release create vX.Y.Z`. The Release
publish event is what fires
[`pkgdown.yml`](../../.github/workflows/pkgdown.yml)'s
`release: published` trigger and deploys the site for that release.

## Distribution

The package is currently installed from GitHub (see
[`README.md`](../../README.md)):

```r
remotes::install_github("hms-dbmi/pic-sure-r-adapter-hpds@<ref>")
```

CRAN submission status is **uncertain — confirm with maintainers**
before doing any CRAN-related work. If CRAN is in scope, the flow is:

```r
devtools::release()
```

which runs `R CMD check --as-cran`, prompts through the submission
checklist, and uploads the tarball. The package must pass
`R CMD check --as-cran` clean (no NOTEs, WARNINGs, or ERRORs).

## pkgdown site

The site is published from the `gh-pages` branch by
[`pkgdown.yml`](../../.github/workflows/pkgdown.yml) on pushes to the
default branches, on `release: published`, and on
`workflow_dispatch`. The published URL is
`https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/` (configured in
[`_pkgdown.yml`](../../_pkgdown.yml) `url:`).

The workflow runs `pkgdown::build_site_github_pages(new_process =
FALSE, install = FALSE)` and deploys via
`JamesIves/github-pages-deploy-action` with `clean: false` so per-
ref builds can coexist.

## Post-release

Bump `DESCRIPTION` `Version:` to the next development version. By R
convention, append `.9000`:

```
# Was: 1.2.3
Version: 1.2.4.9000
```

Commit and push to the development branch.
