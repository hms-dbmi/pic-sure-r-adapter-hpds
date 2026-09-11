# Shared interpreter probe for the tests that cross the reticulate boundary.
#
# `inherits(picsure_py, "python.builtin.module")` cannot serve as the probe:
# `import(delay_load = TRUE)` returns a proxy already carrying that class
# before any interpreter exists, so the check passes and the test then dies on
# its first attribute access with an opaque "Installation of Python not
# found". Resolution has to be forced with `py_available(initialize = TRUE)`,
# and that call can *throw* instead of returning FALSE — reticulate
# provisions its own environment through uv, which errors out of
# `uv_get_or_create_env()` when a pinned requirement will not resolve — so it
# is wrapped and a throw counts as unavailable.
#
# The answer is memoized. Probing is slow, and each failed attempt reprints
# reticulate's entire provisioning diagnostic.

.python_probe <- new.env(parent = emptyenv())

python_status <- function() {
  if (!is.null(.python_probe$status)) {
    return(.python_probe$status)
  }

  reason <- NULL
  available <- isTRUE(tryCatch(
    reticulate::py_available(initialize = TRUE),
    error = function(e) {
      reason <<- conditionMessage(e)
      FALSE
    }
  ))

  module <- FALSE
  if (available) {
    module <- isTRUE(tryCatch(
      reticulate::py_module_available("picsure"),
      error = function(e) {
        reason <<- conditionMessage(e)
        FALSE
      }
    ))
  }

  .python_probe$status <- list(
    available   = available,
    module      = module,
    reason      = reason,
    interpreter = .python_interpreter_looked_for(available)
  )
  .python_probe$status
}

.python_interpreter_looked_for <- function(available) {
  if (available) {
    return(tryCatch(reticulate::py_config()$python, error = function(e) "unknown"))
  }
  configured <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  if (nzchar(configured)) {
    return(configured)
  }
  paste0(
    "none configured - reticulate tried to provision its own uv-managed ",
    "environment for '", picsure:::.PICSURE_PY_SPEC, "'"
  )
}

# The message the one loud environment failure carries. It has to be
# unmistakably about a missing interpreter rather than about disagreeing
# enums, because those two causes were indistinguishable before.
python_unavailable_message <- function(status) {
  configured <- Sys.getenv("RETICULATE_PYTHON", unset = "")
  paste0(
    "Python is NOT available in this environment. Enum parity between the R ",
    "and Python picsure adapters was therefore NOT verified - no enum was ",
    "compared, and this failure is not an enum mismatch.\n",
    "  interpreter looked for: ", status$interpreter, "\n",
    "  RETICULATE_PYTHON:      ", if (nzchar(configured)) configured else "<unset>", "\n",
    "  reticulate reported:    ",
    if (is.null(status$reason)) {
      "could not initialize an interpreter (its provisioning diagnostic is in the log above)"
    } else {
      status$reason
    }, "\n",
    "Point RETICULATE_PYTHON at an interpreter that has the pinned picsure ",
    "package installed and re-run to verify parity."
  )
}

# Same shape, for an interpreter that resolved but cannot import picsure.
# Parity is equally unverified, so it is equally loud.
python_module_unavailable_message <- function(status) {
  paste0(
    "The Python picsure package is NOT importable in this environment. Enum ",
    "parity between the R and Python picsure adapters was therefore NOT ",
    "verified - no enum was compared, and this failure is not an enum ",
    "mismatch.\n",
    "  interpreter used: ", status$interpreter, "\n",
    "  pinned spec:      ", picsure:::.PICSURE_PY_SPEC, "\n",
    "Install the pinned picsure package into that interpreter and re-run to ",
    "verify parity."
  )
}

# Guard for the parity tests. The environment itself is asserted once, loudly,
# by the first test in test-enums-parity.R; the individual comparisons skip so
# a missing interpreter produces one clear failure instead of eight opaque
# ones.
skip_unless_python_parity <- function() {
  status <- python_status()
  if (!isTRUE(status$available) || !isTRUE(status$module)) {
    testthat::skip(
      "Python picsure adapter unavailable - see the failing environment check in test-enums-parity.R."
    )
  }
}

skip_unless_python_interpreter <- function() {
  if (!isTRUE(python_status()$available)) {
    testthat::skip("No Python interpreter available to reticulate.")
  }
}

skip_unless_python_module <- function(module = "picsure") {
  skip_unless_python_interpreter()
  importable <- isTRUE(tryCatch(
    reticulate::py_module_available(module),
    error = function(e) FALSE
  ))
  if (!importable) {
    testthat::skip(paste0("Python module '", module, "' is not importable."))
  }
}
