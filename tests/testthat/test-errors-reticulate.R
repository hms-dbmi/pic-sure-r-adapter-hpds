# Error-boundary tests that cross the real reticulate boundary.
#
# A fake cannot stand in for a reticulate error condition. The real one is
# classed with the Python exception's whole MRO plus `python.builtin.object`
# and `python.builtin.BaseException`, and it carries a `py_object` attribute
# that reticulate's `$` and `[[` methods dereference. A hand-built condition
# with those classes and no `py_object` cannot even be passed to `stop()`,
# because reticulate's `$` throws while `stop()` reads `conditionMessage()`.
# So the message unwrapping and class mapping in R/errors.R is only fully
# exercised here, against exceptions raised by Python.
#
# The raw reticulate condition carries both plumbing layers, the class prefix
# and the `py_last_error()` footer. One test below asserts that directly, so
# the defect the unwrapping addresses stays visible.

# Raises a named exception from the Python `picsure.errors` module.
#
# The structured consent errors take five constructor arguments. Everything
# else takes the message alone.
picsure_raise <- function(name, message = "the human-readable message") {
  reticulate::py_run_string(
    "
import picsure.errors as _E


def _picsure_raise(name, message):
    cls = getattr(_E, name)
    if name in ('PicSureConsentDeniedError', 'PicSureConsentLookupError'):
        raise cls(403, '{}', 'consent_denied', 'the server said so', message)
    raise cls(message)
",
    convert = TRUE
  )
  reticulate::py$`_picsure_raise`(name, message)
}

# Whether the installed Python build defines the named exception class.
#
# The pinned Python build predates part of the hierarchy, so the mapping tests
# assert only on the classes the installed build defines. Skipping absent
# ones is what lets those tests outlive a pin bump in either direction.
python_error_defines <- function(name) {
  isTRUE(tryCatch(
    reticulate::py_has_attr(reticulate::import("picsure.errors"), name),
    error = function(e) FALSE
  ))
}

test_that("a real Python exception reaches R without any Python plumbing", {
  skip_unless_python_module()

  err <- tryCatch(
    with_picsure_error(picsure_raise("PicSureError", "Your token expired on 2026-03-14.")),
    error = function(e) e
  )

  expect_s3_class(err, "picsureError")
  expect_equal(conditionMessage(err), "Your token expired on 2026-03-14.")
  expect_false(grepl("picsure.errors", conditionMessage(err), fixed = TRUE))
  expect_false(grepl("py_last_error", conditionMessage(err), fixed = TRUE))
  expect_false(grepl("PicSureError", conditionMessage(err), fixed = TRUE))
})

test_that("the raw reticulate condition really does carry both plumbing layers", {
  skip_unless_python_module()

  raw <- tryCatch(picsure_raise("PicSureError", "Your token expired."), error = function(e) e)

  expect_true(startsWith(conditionMessage(raw), "picsure.errors.PicSureError: "))
  expect_match(conditionMessage(raw), "py_last_error", fixed = TRUE)
})

test_that("the Python exception stays reachable on the R condition", {
  skip_unless_python_module()

  err <- tryCatch(
    with_picsure_error(picsure_raise("PicSureQueryError")),
    error = function(e) e
  )

  expect_equal(err$python_class, "picsure.errors.PicSureQueryError")
  expect_s3_class(err$py_cause, "python.builtin.Exception")
})

test_that("options(picsure.python_detail) restores the Python class in the message", {
  skip_unless_python_module()

  err <- with_picsure_options(
    list(picsure.python_detail = TRUE),
    tryCatch(with_picsure_error(picsure_raise("PicSureQueryError", "no such concept")),
             error = function(e) e)
  )

  expect_match(conditionMessage(err), "no such concept", fixed = TRUE)
  expect_match(conditionMessage(err), "picsure.errors.PicSureQueryError", fixed = TRUE)
})

test_that("each Python error class maps to its R condition class", {
  skip_unless_python_module()

  expected <- c(
    PicSureError                = "picsureError",
    PicSureAuthError            = "picsureAuthError",
    PicSureAuthenticationError  = "picsureAuthenticationError",
    PicSureAuthorizationError   = "picsureAuthorizationError",
    PicSureConsentDeniedError   = "picsureConsentDeniedError",
    PicSureConnectionError      = "picsureConnectionError",
    PicSureTLSError             = "picsureTLSError",
    PicSureServerError          = "picsureServerError",
    PicSureConsentLookupError   = "picsureConsentLookupError",
    PicSureQueryError           = "picsureQueryError",
    PicSureValidationError      = "picsureValidationError"
  )

  present <- names(expected)[vapply(names(expected), python_error_defines, logical(1))]
  expect_gt(length(present), 0L)

  for (name in present) {
    err <- tryCatch(with_picsure_error(picsure_raise(name)), error = function(e) e)
    expect_s3_class(err, expected[[name]])
    expect_s3_class(err, "picsureError")
    expect_equal(conditionMessage(err), "the human-readable message")
  }
})

test_that("every mapped Python auth class is catchable as picsureAuthError", {
  skip_unless_python_module()

  auth_classes <- c(
    "PicSureAuthError", "PicSureAuthenticationError",
    "PicSureAuthorizationError", "PicSureConsentDeniedError"
  )
  present <- auth_classes[vapply(auth_classes, python_error_defines, logical(1))]
  expect_gt(length(present), 0L)

  for (name in present) {
    caught <- tryCatch(
      with_picsure_error(picsure_raise(name)),
      picsureAuthError = function(e) TRUE,
      error            = function(e) FALSE
    )
    expect_true(caught, info = name)
  }
})

test_that("a structured consent error keeps the server's own account of it", {
  skip_unless_python_module()

  err <- tryCatch(
    with_picsure_error(picsure_raise("PicSureConsentDeniedError", "consents do not cover this")),
    picsureError = function(e) e
  )

  expect_s3_class(err, "picsureConsentDeniedError")
  expect_equal(conditionMessage(err), "consents do not cover this")
  expect_equal(err$py_cause$error_type, "consent_denied")
  expect_equal(err$py_cause$server_message, "the server said so")
})

test_that("a Python builtin exception also loses its framing", {
  skip_unless_python_module()

  reticulate::py_run_string("
def _picsure_raise_builtin():
    raise ValueError('not a valid concept path')
")

  err <- tryCatch(
    with_picsure_error(reticulate::py$`_picsure_raise_builtin`()),
    error = function(e) e
  )

  expect_s3_class(err, "picsureError")
  expect_equal(conditionMessage(err), "not a valid concept path")
})

# Every public exception class the pinned `picsure.errors` module defines.
#
# Listed Python-side and filtered to real subclasses of `PicSureError`, so a
# helper or an imported name in that module cannot be mistaken for one.
python_error_class_names <- function() {
  reticulate::py_run_string(
    "
import inspect
import picsure.errors as _E


def _picsure_error_classes():
    return sorted(
        name
        for name, obj in vars(_E).items()
        if not name.startswith('_')
        and inspect.isclass(obj)
        and issubclass(obj, _E.PicSureError)
    )
"
  )
  reticulate::py$`_picsure_error_classes`()
}

test_that("every exception class the pinned build defines is mapped to a condition class", {
  skip_unless_python_module()

  defined <- python_error_class_names()
  expect_true("PicSureError" %in% defined)

  mapped <- sub(
    "^picsure\\.errors\\.", "",
    names(picsure:::.PICSURE_PY_CONDITION_CLASSES)
  )
  unmapped <- setdiff(defined, c(mapped, "PicSureError"))

  expect_equal(
    unmapped, character(0),
    info = paste0(
      "The pinned picsure.errors module defines ",
      paste(unmapped, collapse = ", "),
      ", which .PICSURE_PY_CONDITION_CLASSES in R/errors.R does not map. An ",
      "unmapped class arrives as a bare picsureError, so a handler written ",
      "for the specific failure, say tryCatch(picsureConnectionError = ",
      "retry), quietly stops firing. Add the module-qualified name to ",
      ".PICSURE_PY_CONDITION_CLASSES, and add an ancestry row for the R ",
      "class it maps to in .PICSURE_CONDITION_PARENTS, which is a separate ",
      "table and the one that is easy to forget. PicSureError is excluded ",
      "here on purpose: it is the base class, and an exception raised as ",
      "exactly PicSureError is meant to reach R as a plain picsureError."
    )
  )
})
