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

# Pinned Python dependency. Bump this together with each R release.
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
