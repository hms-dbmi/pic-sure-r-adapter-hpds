# zzz.R
#
# This file uses a standard R package convention: filenames prefixed with "zzz"
# sort last alphabetically, so the .onLoad and .onAttach hooks defined here
# run after every other function in the package has been sourced. This file is
# where we declare the Python `picsure` dependency and create the lazy module
# handle that every wrapper in the package calls into.

#' @keywords internal
"_PACKAGE"

# Package-private handle to the Python `picsure` module. Wrappers reference
# this object; tests swap it via testthat::local_mocked_bindings().
picsure_py <- NULL

# Pinned Python dependency.
#
# The pin is intentional and exact (==, not >=). The Python `picsure` package
# is pre-1.0 and reserves the right to break behavior across patch releases.
# Bumping this version requires:
#   1. Re-running the full integration suite under VPN against a backend
#      that ships the matching server protocol.
#   2. Updating any wrapper signatures whose Python kwargs changed.
# Once `picsure` reaches 1.0, relax to a compatible range (e.g. "picsure>=1.0,<2").
#
# Note: 0.1.0-on-`main` of `pic-sure-python-adapter-hpds` includes
# QueryType (merged 2026-04-29 without a version bump). A published
# 0.1.0 on PyPI predates QueryType. Until Python publishes a new
# release, this pin only resolves correctly against a local install of
# `pic-sure-python-adapter-hpds@main`. CI / fresh PyPI installs of
# 0.1.0 will fail the QueryType drift test (tests/testthat/test-enums-parity.R).
.PICSURE_PY_SPEC <- "picsure==0.1.0"

.onLoad <- function(libname, pkgname) {
  reticulate::py_require(.PICSURE_PY_SPEC)

  # delay_load = TRUE: the Python env isn't provisioned until the first real
  # attribute access on picsure_py. `library(picsure)` stays fast; tests that
  # never touch Python never trigger env creation.
  picsure_py <<- reticulate::import("picsure", delay_load = TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "picsure loaded. On first call, reticulate will provision an isolated ",
    "Python environment; this takes a few seconds the first time only."
  )
}
