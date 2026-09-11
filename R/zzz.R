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
# SHA is the head of the Python `pic_sure_api_rewrite` branch, and is the
# squash-merge of PR #40 ("[ALS-12763] consent error handling").
#
# The SHA MUST name a commit reachable from a branch, never a pull-request
# head. GitHub publishes PR heads under refs/pull/<n>/head, so `uv pip
# install` and any tool that fetches the SHA directly resolve one and look
# correct. reticulate's provisioning can instead fall back to fetching the
# default refspec — branches and tags only — and then run
# `git rev-parse <sha>^0`, which exits 128 for a commit no branch contains.
# A PR-head pin therefore fails exactly where real users hit it, cannot be
# satisfied from cache or offline at all, and disappears outright if the pull
# request is deleted. A squash-merge produces a NEW commit: read the SHA back
# off the branch after merging instead of reusing the one from the PR, and
# confirm it with `git branch -r --contains <sha>` before pinning it.
#
# Bumping the pinned ref requires:
#   1. Re-running the full integration suite under VPN against a backend
#      that ships the matching server protocol.
#   2. Updating any wrapper signatures whose Python kwargs changed.
.PICSURE_PY_SPEC <- "picsure @ git+https://github.com/hms-dbmi/pic-sure-python-adapter-hpds.git@0eec30d062751396e006284e79c19110408e4a62"

# Records whether the pinned-build check has already run, so the warning is
# emitted at most once per session.
.picsure_pin_check <- new.env(parent = emptyenv())

# Reads the 40-character commit SHA off a PEP 508 requirement string.
#
# Returns NA_character_ when the spec pins no commit — a version-specifier pin
# against a package index, say, which has no commit to compare and so cannot be
# checked this way.
.picsure_pinned_sha <- function(spec = .PICSURE_PY_SPEC) {
  found <- regexpr("[0-9a-f]{40}$", spec, perl = TRUE)
  if (found[[1L]] < 0L) NA_character_ else regmatches(spec, found)
}

# Reads the commit SHA encoded in an installed distribution's version string.
#
# The Python adapter derives its version from `git describe` via `hatch-vcs`, so
# a build installed from a commit carries an abbreviation of that commit in its
# local version segment, as in `2.0.1.dev11+g0eec30d06`. Returns NA_character_
# when the version carries none, which is what an index release or a build from
# an sdist looks like.
.picsure_build_sha <- function(version) {
  if (length(version) != 1L || !is.character(version) || is.na(version)) {
    return(NA_character_)
  }
  found <- regexpr("[+.]g[0-9a-f]{7,40}", version, perl = TRUE)
  if (found[[1L]] < 0L) {
    NA_character_
  } else {
    sub("^[+.]g", "", regmatches(version, found))
  }
}

# Reports whether reticulate has already initialized an interpreter, which is
# what decides which of two load paths `reticulate::import()` takes below.
# Asking with `initialize = FALSE` reads the state without starting Python, so
# this is safe to call from `.onLoad`.
.picsure_python_already_initialized <- function() {
  isTRUE(tryCatch(
    reticulate::py_available(initialize = FALSE),
    error = function(e) FALSE
  ))
}

# Warns when the Python `picsure` build that actually loaded is not the pinned
# commit. Compares the commit encoded in the installed distribution's version,
# read through `importlib.metadata`, against the SHA in `.PICSURE_PY_SPEC`, and
# returns TRUE invisibly only on a match. Runs at most once per session.
#
# Nothing else in the package notices when the module that resolves is not the
# pinned one: an already-active virtual environment takes precedence over the
# pin whenever reticulate attaches to it, which is how entire test campaigns ran
# against an unpinned build without a single warning. This makes that
# substitution visible.
#
# Deliberately never signals an error. A local override is a supported
# development workflow, so disagreement is reported and execution continues.
# Every step is guarded for the same reason: this runs from a reticulate load
# hook and must not turn a working interpreter into an apparently broken one.
.picsure_warn_on_pin_mismatch <- function() {
  if (isTRUE(.picsure_pin_check$done)) {
    return(invisible(isTRUE(.picsure_pin_check$matched)))
  }
  .picsure_pin_check$done <- TRUE
  .picsure_pin_check$matched <- FALSE

  expected <- tryCatch(.picsure_pinned_sha(), error = function(e) NA_character_)
  if (is.na(expected)) {
    return(invisible(FALSE))
  }

  installed <- tryCatch(
    reticulate::import("importlib.metadata")$version("picsure"),
    error = function(e) NA_character_
  )
  actual <- .picsure_build_sha(installed)

  if (!is.na(actual) && startsWith(expected, actual)) {
    .picsure_pin_check$matched <- TRUE
    return(invisible(TRUE))
  }

  loaded_from <- tryCatch(
    reticulate::import("picsure")$`__file__`,
    error = function(e) NA_character_
  )

  packageStartupMessage(
    if (is.na(actual)) {
      "picsure: could not confirm that the Python 'picsure' build which loaded is the pinned one.\n"
    } else {
      "picsure: the Python 'picsure' build which loaded is NOT the pinned one.\n"
    },
    "  expected commit: ", expected, "\n",
    "  actual version:  ",
    if (is.na(installed)) "unknown (no distribution metadata found)" else installed, "\n",
    "  loaded from:     ", if (is.na(loaded_from)) "unknown" else loaded_from, "\n",
    "Behavior may differ from the build this R package was tested against. ",
    "Unset RETICULATE_PYTHON to let reticulate provision the pinned build."
  )
  invisible(FALSE)
}

.onLoad <- function(libname, pkgname) {
  reticulate::py_require(.PICSURE_PY_SPEC)

  # delay_load: the Python env isn't provisioned until the first real
  # attribute access on picsure_py. `library(picsure)` stays fast; tests that
  # never touch Python never trigger env creation. The on_load hook runs the
  # pinned-build check once the module has actually resolved.
  picsure_py <<- reticulate::import(
    "picsure",
    delay_load = list(on_load = .picsure_warn_on_pin_mismatch)
  )

  if (.picsure_python_already_initialized()) {
    .picsure_warn_on_pin_mismatch()
  }
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "picsure loaded. On first call, reticulate will provision an isolated ",
    "Python environment; this takes a few seconds the first time only."
  )
}
