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

# RR-5: reticulate's framing is stripped from the message

test_that("with_picsure_error strips reticulate's class prefix and footer", {
  framed <- structure(
    list(message = paste0(
      "picsure.errors.PicSureAuthError: Your PIC-SURE token was rejected.\n",
      "Run `reticulate::py_last_error()` for details."
    )),
    class = c("picsure.errors.PicSureAuthError", "picsure.errors.PicSureError",
              "python.builtin.Exception", "error", "condition")
  )

  err <- tryCatch(with_picsure_error(stop(framed)), error = function(e) e)

  expect_equal(conditionMessage(err), "Your PIC-SURE token was rejected.")
  expect_false(grepl("picsure.errors", conditionMessage(err), fixed = TRUE))
  expect_false(grepl("py_last_error", conditionMessage(err), fixed = TRUE))
})

test_that("the Python origin stays reachable on the condition", {
  framed <- structure(
    list(message = "picsure.errors.PicSureQueryError: The server rejected the query."),
    class = c("picsure.errors.PicSureQueryError", "picsure.errors.PicSureError",
              "python.builtin.Exception", "error", "condition")
  )

  err <- tryCatch(with_picsure_error(stop(framed)), error = function(e) e)

  expect_identical(err$py_cause, framed)
  expect_equal(err$python_class, "picsure.errors.PicSureQueryError")
})

test_that("options(picsure.python_detail) puts the Python class back in the message", {
  framed <- structure(
    list(message = "picsure.errors.PicSureQueryError: The server rejected the query."),
    class = c("picsure.errors.PicSureQueryError", "picsure.errors.PicSureError",
              "python.builtin.Exception", "error", "condition")
  )

  with_picsure_options(list(picsure.python_detail = TRUE), {
    err <- tryCatch(with_picsure_error(stop(framed)), error = function(e) e)
  })

  expect_match(conditionMessage(err), "The server rejected the query.", fixed = TRUE)
  expect_match(conditionMessage(err), "picsure.errors.PicSureQueryError", fixed = TRUE)
  expect_match(conditionMessage(err), "py_last_error", fixed = TRUE)
})

test_that("a message whose own text is colon-shaped is not truncated", {
  framed <- structure(
    list(message = "picsure.errors.PicSureError: Note: consents were not applied."),
    class = c("picsure.errors.PicSureError", "python.builtin.Exception",
              "error", "condition")
  )

  err <- tryCatch(with_picsure_error(stop(framed)), error = function(e) e)

  expect_equal(conditionMessage(err), "Note: consents were not applied.")
})

test_that("an R condition carrying no Python object has no python_class", {
  err <- picsureError("plain")
  expect_null(err$python_class)
  expect_null(err$py_cause)
})

# RL-13: the condition hierarchy mirrors the Python one

test_that("picsureError() expands a class to its documented ancestors", {
  expect_identical(
    class(picsureError("m", class = "picsureConsentDeniedError")),
    c("picsureConsentDeniedError", "picsureAuthorizationError", "picsureAuthError",
      "picsureError", "error", "condition")
  )
  expect_identical(
    class(picsureError("m", class = "picsureConsentLookupError")),
    c("picsureConsentLookupError", "picsureServerError", "picsureConnectionError",
      "picsureError", "error", "condition")
  )
  expect_identical(
    class(picsureError("m", class = "picsureAuthenticationError")),
    c("picsureAuthenticationError", "picsureAuthError",
      "picsureError", "error", "condition")
  )
  expect_identical(
    class(picsureError("m")),
    c("picsureError", "error", "condition")
  )
})

test_that("every condition class in the hierarchy is catchable as picsureError", {
  for (cls in names(picsure:::.PICSURE_CONDITION_PARENTS)) {
    err <- tryCatch(stop(picsureError("m", class = cls)), picsureError = function(e) e)
    expect_s3_class(err, cls)
    expect_s3_class(err, "picsureError")
  }
})

test_that("an authentication problem is catchable as the general auth condition", {
  # PSAMA answers 403, not 401, for a bad token on /user/me, so a server-side
  # rejection is an authorization error and a locally-detected one an
  # authentication error. A handler for "this is a token problem" has to catch
  # both, which is what picsureAuthError is for.
  for (cls in c("picsureAuthenticationError", "picsureAuthorizationError")) {
    caught <- tryCatch(
      stop(picsureError("token trouble", class = cls)),
      picsureAuthError = function(e) class(e)[[1L]]
    )
    expect_equal(caught, cls)
  }
})

test_that("the Python exception class picks the R condition class", {
  cases <- list(
    list("picsure.errors.PicSureAuthenticationError", "picsureAuthenticationError"),
    list("picsure.errors.PicSureAuthorizationError",  "picsureAuthorizationError"),
    list("picsure.errors.PicSureAuthError",           "picsureAuthError"),
    list("picsure.errors.PicSureTLSError",            "picsureTLSError"),
    list("picsure.errors.PicSureServerError",         "picsureServerError"),
    list("picsure.errors.PicSureConnectionError",     "picsureConnectionError"),
    list("picsure.errors.PicSureQueryError",          "picsureQueryError"),
    list("picsure.errors.PicSureValidationError",     "picsureValidationError")
  )
  for (case in cases) {
    fake <- structure(
      list(message = paste0(case[[1L]], ": something went wrong")),
      class = c(case[[1L]], "picsure.errors.PicSureError",
                "python.builtin.Exception", "error", "condition")
    )
    err <- tryCatch(with_picsure_error(stop(fake)), error = function(e) e)
    expect_s3_class(err, case[[2L]])
    expect_equal(conditionMessage(err), "something went wrong")
  }
})

test_that("the most specific Python class in the MRO wins", {
  # A consent refusal under the current Python hierarchy carries
  # PicSureAuthorizationError and PicSureAuthError in its MRO too; the R
  # condition must be the consent one, not one of its ancestors.
  fake <- structure(
    list(message = "picsure.errors.PicSureConsentDeniedError: consents do not cover this"),
    class = c("picsure.errors.PicSureConsentDeniedError",
              "picsure.errors.PicSureAuthorizationError",
              "picsure.errors.PicSureAuthError",
              "picsure.errors.PicSureError",
              "python.builtin.Exception", "error", "condition")
  )
  err <- tryCatch(with_picsure_error(stop(fake)), error = function(e) e)
  expect_identical(class(err)[[1L]], "picsureConsentDeniedError")
})

test_that("the pinned Python build's flatter hierarchy still maps cleanly", {
  # The pinned build puts PicSureConsentDeniedError directly under
  # PicSureError and has no PicSureAuthorizationError at all. The R side
  # re-derives the ancestry from its own table, so the R condition has the
  # documented shape either way.
  fake <- structure(
    list(message = "picsure.errors.PicSureConsentDeniedError: consents do not cover this"),
    class = c("picsure.errors.PicSureConsentDeniedError",
              "picsure.errors.PicSureError",
              "python.builtin.Exception", "error", "condition")
  )
  err <- tryCatch(with_picsure_error(stop(fake)), picsureAuthError = function(e) e)
  expect_s3_class(err, "picsureConsentDeniedError")
  expect_s3_class(err, "picsureAuthorizationError")
})

# RL-12: the package's own argument validation raises picsureErrors

test_that("argument validation raises a picsureValidationError, not a simpleError", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)
  df <- data.frame(a = 1)

  rejections <- list(
    function() picsure::searchDictionary(bdc, c("a", "b")),
    function() picsure::searchGenomicValues(bdc, ""),
    function() picsure::searchGenomicValues(bdc, "Gene_with_variant", page = 0),
    function() picsure::runQuery(bdc),
    function() picsure::loadQueryByID(bdc, 123),
    function() picsure::runQueryByID(bdc, ""),
    function() picsure::removeSubQuery(query = "Q"),
    function() picsure::replaceClause(query = "Q"),
    function() picsure::saveQueryByName(bdc, "Q", ""),
    function() picsure::saveQueryByName(bdc, "Q", "n", overwrite = NA),
    function() picsure::exportAsPFB(bdc, "Q"),
    function() picsure::exportCSV(bdc, list(a = 1), tempfile()),
    function() picsure::exportTSV(bdc, df),
    function() picsure::addFacet(fs, c("a", "b"), "v"),
    function() picsure::removeFacet(fs, "study_ids"),
    function() picsure::buildClause(type = "FILTER"),
    function() picsure::buildClause("x"),
    function() picsure::buildClause("x", type = "FILTER", min = "low"),
    function() picsure::buildClauseGroup(list()),
    function() picsure::buildGenomicFilter(values = "x"),
    function() picsure::buildQuery(includeConcepts = list(1, 2)),
    function() picsure::connect(),
    function() picsure::connect(platform = "BDC Authorized"),
    function() picsure::connect(platform = "https://picsure.test", token = "t", nope = 1)
  )

  for (i in seq_along(rejections)) {
    err <- tryCatch(rejections[[i]](), condition = function(e) e)
    expect_s3_class(err, "picsureError")
    expect_s3_class(err, "error")
    expect_false(inherits(err, "simpleError"),
                 info = paste("rejection", i, "was a bare simpleError"))
  }
})

test_that(".picsure_raw_message survives a condition reticulate cannot read", {
  # A condition classed python.builtin.object whose Python object is missing:
  # reticulate's `$` throws for it, so `conditionMessage()` throws too.
  # Building an error message must not fail, or the researcher's message is
  # replaced by reticulate's internals.
  malformed <- structure(
    list(message = "the real message"),
    class = c("python.builtin.Exception", "python.builtin.object",
              "error", "condition")
  )
  expect_equal(picsure:::.picsure_raw_message(malformed), "the real message")

  no_message <- structure(
    list(),
    class = c("python.builtin.Exception", "python.builtin.object",
              "error", "condition")
  )
  expect_equal(picsure:::.picsure_raw_message(no_message), "")
})
