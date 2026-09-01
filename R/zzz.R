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
# Installed directly from the upstream GitHub repository via PEP 508
# direct-reference syntax (resolved by uv under `reticulate::py_require`).
# `pic-sure-python-adapter-hpds` is not yet on PyPI.
#
# This R rewrite branch REQUIRES the Python rewrite line: the R adapter has no
# HTTP code of its own, so backend compatibility is whatever this ref resolves
# to. `@main` is pre-rewrite (discovers resources via /picsure/info/resources
# and 404s at connect() on the rewrite gateway) — it MUST NOT be used here.
# This tracks the Python branch carrying the corrected rewrite gateway paths
# (the /picsure prefix, /picsure/dictionary/*, versioned open queries, etc.).
#
# Pinned to an immutable commit SHA, not a branch: a moving branch is how the
# earlier drift (R silently pinned at pre-rewrite @main) went unnoticed. This
# SHA is the head of the Python `pic_sure_api_rewrite` branch carrying the
# corrected gateway paths.
#
# Bumping the pinned ref requires:
#   1. Re-running the full integration suite under VPN against a backend
#      that ships the matching server protocol.
#   2. Updating any wrapper signatures whose Python kwargs changed.
.PICSURE_PY_SPEC <- "picsure @ git+https://github.com/hms-dbmi/pic-sure-python-adapter-hpds.git@a023f3678254ede43c9fee966f20070d3372ee11"

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
