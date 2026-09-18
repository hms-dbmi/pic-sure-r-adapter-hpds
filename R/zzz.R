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
# SHA is the head of the Python `pic_sure_api_rewrite` branch, the
# squash-merge of PR #40 ("[ALS-12763] consent error handling").
#
# The SHA must name a commit reachable from a branch, never a pull-request
# head. GitHub publishes PR heads under refs/pull/<n>/head, so `uv pip
# install` and any tool that fetches the SHA directly resolve one and look
# correct. reticulate's provisioning can instead fall back to fetching the
# default refspec, which covers branches and tags only, and then run
# `git rev-parse <sha>^0`. That exits 128 for a commit no branch contains.
# A PR-head pin therefore fails for real users, cannot be satisfied from
# cache or offline, and vanishes if the pull request is deleted. A
# squash-merge produces a new commit. Read the SHA back off the branch after
# merging instead of reusing the one from the PR, and confirm it with
# `git branch -r --contains <sha>` before pinning it.
#
# Bumping the pinned ref requires:
#   1. Re-running the full integration suite under VPN against a backend
#      that ships the matching server protocol.
#   2. Updating any wrapper signatures whose Python kwargs changed.
#   3. Setting `.PICSURE_PY_TAG` when the new commit is one a release tag
#      points at, and clearing it to NA_character_ otherwise.
.PICSURE_PY_SPEC <- "picsure @ git+https://github.com/hms-dbmi/pic-sure-python-adapter-hpds.git@0eec30d062751396e006284e79c19110408e4a62"

# Release tag that points at the pinned commit, or NA_character_ when no tag
# does. hatch-vcs versions a build made exactly at a tag as the bare tag
# version with no commit suffix, so the pin check needs the tag to recognize
# such a build as the pinned one. Set it alongside the SHA whenever the pin
# moves to a tagged commit.
.PICSURE_PY_TAG <- NA_character_

# Records whether the pinned-build check has already run, so its report is
# emitted at most once per session.
.picsure_pin_check <- new.env(parent = emptyenv())

# Reads the 40-character commit SHA off a PEP 508 requirement string.
#
# Returns NA_character_ when the spec pins no commit, as with a version
# specifier against a package index, which has no commit to compare.
.picsure_pinned_sha <- function(spec = .PICSURE_PY_SPEC) {
  found <- regexpr("[0-9a-f]{40}$", spec, perl = TRUE)
  if (found[[1L]] < 0L) NA_character_ else regmatches(spec, found)
}

# Resolves the release tag the pin corresponds to, if any.
#
# A recorded `tag` wins, so a SHA pin at a tagged commit is recognized. With
# no recorded tag, a spec whose ref looks like a version tag (`@v2.1.0`)
# yields that ref. Returns NA_character_ when the pin has no tag form.
.picsure_pinned_tag <- function(spec = .PICSURE_PY_SPEC, tag = .PICSURE_PY_TAG) {
  if (length(tag) == 1L && !is.na(tag)) {
    return(tag)
  }
  ref <- sub("^.*@", "", spec)
  looks_like_tag <- grepl("^v?[0-9]+\\.[0-9]+[0-9A-Za-z.]*$", ref, perl = TRUE)
  if (looks_like_tag && is.na(.picsure_pinned_sha(spec))) ref else NA_character_
}

# Normalizes a git tag to the version string hatch-vcs derives from it, which
# drops a leading `v`.
.picsure_tag_version <- function(tag) {
  sub("^v", "", tag)
}

# Drops the local version segment (`+...`) from an installed version string,
# leaving the public version a tagged build reports.
.picsure_release_version <- function(version) {
  sub("\\+.*$", "", version)
}

# Reads the commit SHA encoded in an installed distribution's version string.
#
# The Python adapter derives its version from `git describe` via `hatch-vcs`, so
# a build installed from a commit carries an abbreviation of that commit in its
# local version segment, as in `2.0.1.dev11+g0eec30d06`. Returns NA_character_
# when the version carries none, which is what an index release, a build from
# an sdist, or a build made exactly at a tag looks like.
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

# Decides how an installed version relates to the pin.
#
# Returns "match" when the version carries the pinned commit, or when it is
# the bare version of the pinned tag; "mismatch" when it names a different
# commit or a different release; and "unconfirmed" when there is no version
# or the version carries nothing the pin can be compared against.
.picsure_pin_verdict <- function(installed, expected_sha, expected_tag) {
  if (length(installed) != 1L || is.na(installed)) {
    return("unconfirmed")
  }
  actual_sha <- .picsure_build_sha(installed)
  if (!is.na(actual_sha)) {
    matched <- !is.na(expected_sha) && startsWith(expected_sha, actual_sha)
    return(if (matched) "match" else "mismatch")
  }
  if (is.na(expected_tag)) {
    return("unconfirmed")
  }
  tagged <- identical(
    .picsure_release_version(installed),
    .picsure_tag_version(expected_tag)
  )
  if (tagged) "match" else "mismatch"
}

# Builds the text reported for a "mismatch" or "unconfirmed" verdict: what
# the pin expects, what loaded, where it loaded from, and the documented way
# back to the pinned build.
.picsure_pin_report <- function(verdict, expected_sha, expected_tag, installed, loaded_from) {
  expected <- if (is.na(expected_tag)) {
    expected_sha
  } else if (is.na(expected_sha)) {
    expected_tag
  } else {
    paste0(expected_tag, " (", expected_sha, ")")
  }
  paste0(
    if (identical(verdict, "mismatch")) {
      "picsure: the Python 'picsure' build which loaded is NOT the pinned one.\n"
    } else {
      "picsure: could not confirm that the Python 'picsure' build which loaded is the pinned one.\n"
    },
    "  expected:        ", expected, "\n",
    "  actual version:  ",
    if (is.na(installed)) "unknown (no distribution metadata found)" else installed, "\n",
    "  loaded from:     ", if (is.na(loaded_from)) "unknown" else loaded_from, "\n",
    "Behavior may differ from the build this R package was tested against. ",
    "Unset RETICULATE_PYTHON and any other preselected Python, then start a ",
    "fresh R session so reticulate provisions the pinned build into its ",
    "uv-managed environment, as described in vignette('getting-started')."
  )
}

# Reads the installed `picsure` distribution's version through
# `importlib.metadata`. Requires an initialized interpreter.
.picsure_installed_version <- function() {
  reticulate::import("importlib.metadata")$version("picsure")
}

# Reads the file path the Python `picsure` module was imported from. Requires
# an initialized interpreter.
.picsure_module_path <- function() {
  reticulate::import("picsure")$`__file__`
}

# Reports whether reticulate has already initialized an interpreter, which
# decides which load path `reticulate::import()` takes in `.onLoad`. Asking
# with `initialize = FALSE` reads the state without starting Python, so this
# is safe to call from `.onLoad`.
.picsure_python_already_initialized <- function() {
  isTRUE(tryCatch(
    reticulate::py_available(initialize = FALSE),
    error = function(e) FALSE
  ))
}

# Checks that the Python `picsure` build which actually loaded is the pinned
# one, and reports when it is not. A build naming a different commit or
# release raises a warning. A build that cannot be compared, because it has no
# distribution metadata or a version with no commit suffix and no tag to
# check against, emits a package startup message instead. Returns TRUE
# invisibly only on a match. Runs at most once per `state`.
#
# Nothing else in the package notices when the module that resolves is not
# the pinned one. An already-active virtual environment takes precedence over
# the pin whenever reticulate attaches to it, which is how entire test
# campaigns ran against an unpinned build without any notice. This makes that
# substitution visible.
#
# Deliberately never signals an error. A local override is a supported
# development workflow, so disagreement is reported and execution continues.
# Every reader is guarded for the same reason. This runs from a reticulate
# load hook and must not turn a working interpreter into an apparently broken
# one.
#
# @param spec PEP 508 requirement string the pin lives in.
# @param tag Release tag pointing at the pinned commit, or NA_character_.
# @param installed_version Function returning the installed distribution's
#   version string. Swapped in tests.
# @param module_path Function returning the path the module loaded from.
#   Swapped in tests.
# @param state Environment recording whether the check has run.
.picsure_warn_on_pin_mismatch <- function(
    spec = .PICSURE_PY_SPEC,
    tag = .PICSURE_PY_TAG,
    installed_version = .picsure_installed_version,
    module_path = .picsure_module_path,
    state = .picsure_pin_check) {
  if (isTRUE(state$done)) {
    return(invisible(isTRUE(state$matched)))
  }
  state$done <- TRUE
  state$matched <- FALSE

  expected_sha <- tryCatch(.picsure_pinned_sha(spec), error = function(e) NA_character_)
  expected_tag <- tryCatch(.picsure_pinned_tag(spec, tag), error = function(e) NA_character_)
  if (is.na(expected_sha) && is.na(expected_tag)) {
    return(invisible(FALSE))
  }

  installed <- tryCatch(installed_version(), error = function(e) NA_character_)
  verdict <- .picsure_pin_verdict(installed, expected_sha, expected_tag)
  if (identical(verdict, "match")) {
    state$matched <- TRUE
    return(invisible(TRUE))
  }

  loaded_from <- tryCatch(module_path(), error = function(e) NA_character_)
  report <- .picsure_pin_report(verdict, expected_sha, expected_tag, installed, loaded_from)
  if (identical(verdict, "mismatch")) {
    warning(report, call. = FALSE)
  } else {
    packageStartupMessage(report)
  }
  invisible(FALSE)
}

# Declares the pinned Python dependency and binds the lazy `picsure` module
# handle. The Python environment is not provisioned until the first real
# attribute access on `picsure_py`, so `library(picsure)` stays fast and tests
# that never touch Python never trigger env creation. The `on_load` hook runs
# the pinned-build check once the module has resolved. When Python is already
# initialized, reticulate imports eagerly and skips that hook, so the check
# runs directly instead.
.onLoad <- function(libname, pkgname) {
  reticulate::py_require(.PICSURE_PY_SPEC)

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
