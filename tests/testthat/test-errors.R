test_that("picsureError() creates a condition of the right classes", {
  e <- picsureError("something bad happened")
  expect_s3_class(e, "picsureError")
  expect_s3_class(e, "error")
  expect_s3_class(e, "condition")
  expect_equal(conditionMessage(e), "something bad happened")
  expect_null(e$py_cause)
})

test_that("picsureError() attaches py_cause when given one", {
  cause <- simpleError("python side")
  e <- picsureError("r side message", py_cause = cause)
  expect_identical(e$py_cause, cause)
})

test_that("picsureError() stops cleanly when raised with stop()", {
  err <- tryCatch(stop(picsureError("boom")), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_equal(conditionMessage(err), "boom")
})

test_that("constructor name matches the class so tryCatch handlers work", {
  # Regression: this is the bug the rename fixes. A user writing a handler
  # named after the constructor must catch the resulting condition.
  caught <- FALSE
  tryCatch(
    stop(picsureError("via tryCatch")),
    picsureError = function(e) caught <<- TRUE
  )
  expect_true(caught)
})

test_that("with_picsure_error passes through successful results unchanged", {
  expect_equal(with_picsure_error(42), 42)
  expect_equal(with_picsure_error({ x <- 1; x + 1 }), 2)
})

test_that("with_picsure_error re-raises non-Python R errors unchanged", {
  expect_error(
    with_picsure_error(stop("plain R error")),
    "plain R error",
    class = "simpleError"
  )
})

test_that("with_picsure_error converts a python.builtin.Exception to picsureError", {
  fake_py_exception <- structure(
    list(message = "Your token expired on 2026-03-14."),
    class = c("python.builtin.Exception", "error", "condition")
  )
  err <- tryCatch(
    with_picsure_error(stop(fake_py_exception)),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "token expired", fixed = TRUE)
  expect_identical(err$py_cause, fake_py_exception)
})

test_that("with_picsure_error maps consent_denied to a catchable R-native condition", {
  fake_py_exception <- structure(
    list(
      message = "You have not accepted the required data-use agreement.",
      error_type = "consent_denied"
    ),
    class = c("python.builtin.Exception", "error", "condition")
  )

  err <- tryCatch(
    with_picsure_error(stop(fake_py_exception)),
    picsureError = function(e) e
  )

  expect_s3_class(err, "picsureConsentDeniedError")
  expect_s3_class(err, "picsureError")
  expect_identical(err$py_cause, fake_py_exception)
  expect_equal(conditionMessage(err), "You have not accepted the required data-use agreement.")
})

test_that("with_picsure_error maps consent_lookup_failed to a catchable R-native condition", {
  fake_py_exception <- structure(
    list(
      message = "PIC-SURE could not determine your consent status.",
      error_type = "consent_lookup_failed"
    ),
    class = c("python.builtin.Exception", "error", "condition")
  )

  err <- tryCatch(
    with_picsure_error(stop(fake_py_exception)),
    picsureError = function(e) e
  )

  expect_s3_class(err, "picsureConsentLookupError")
  expect_s3_class(err, "picsureError")
  expect_identical(err$py_cause, fake_py_exception)
  expect_equal(conditionMessage(err), "PIC-SURE could not determine your consent status.")
})

test_that("with_picsure_error leaves unknown or absent error types as picsureError", {
  fake_py_exceptions <- list(
    structure(
      list(
        message = "An unclassified Python error occurred.",
        error_type = "not_a_consent_error"
      ),
      class = c("python.builtin.Exception", "error", "condition")
    ),
    structure(
      list(message = "An unclassified Python error occurred."),
      class = c("python.builtin.Exception", "error", "condition")
    )
  )

  for (fake_py_exception in fake_py_exceptions) {

    err <- tryCatch(
      with_picsure_error(stop(fake_py_exception)),
      error = function(e) e
    )

    expect_s3_class(err, "picsureError")
    expect_false(inherits(err, "picsureConsentDeniedError"))
    expect_false(inherits(err, "picsureConsentLookupError"))
    expect_identical(err$py_cause, fake_py_exception)
  }
})
