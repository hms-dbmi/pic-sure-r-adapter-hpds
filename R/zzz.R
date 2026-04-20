# zzz.R
#
# This file uses a standard R package convention: filenames prefixed with "zzz"
# sort last alphabetically, so the .onLoad and .onAttach hooks defined here
# run after every other function in the package has been sourced. This file is
# where we declare the Python `picsure` dependency and create the lazy module
# handle that every wrapper in the package calls into.
#
# The handle starts as NULL in this scaffold commit; Task 12 replaces the body
# of .onLoad with a reticulate::py_require() + reticulate::import(..., delay_load)
# pair once the wrappers that depend on it exist.

#' @keywords internal
"_PACKAGE"

# Package-private handle to the Python `picsure` module. Wrappers reference
# this object; tests swap it via testthat::local_mocked_bindings().
picsure_py <- NULL

.onLoad <- function(libname, pkgname) {
  # Scaffold: real reticulate setup lands in Task 12.
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "picsure loaded. On first call, reticulate will provision an isolated ",
    "Python environment; this takes a few seconds the first time only."
  )
}
