test_that("with_picsure_error: conditionMessage extracts the user-friendly Python str(), not a traceback", {
  skip_unless_integration()
  live_token()  # ensure credentials exist; otherwise self-skip

  # Trigger a real PicSureError by connecting with a deliberately invalid token.
  # The platform must exist; the token must be malformed enough to fail auth
  # immediately rather than time out.
  err <- tryCatch(
    picsure::connect(
      platform = live_platform(),
      token    = "definitely-invalid-token-for-error-extraction-probe"
    ),
    error = function(e) e
  )

  expect_s3_class(err, "picsureError")

  # conditionMessage(e) reads from the captured R condition, which reticulate
  # populates with str(exception). For a PicSureAuthError, that's the carefully
  # crafted user-facing message ("Your token is invalid or expired..."). This
  # is what we want users to see.
  msg <- conditionMessage(err)
  expect_true(nzchar(msg), info = "conditionMessage must be non-empty")
  expect_false(
    startsWith(msg, "Traceback"),
    info = "conditionMessage must not be a Python traceback — it should be the user-friendly str()."
  )

  # Document the divergence from reticulate::py_last_error()$message: the
  # latter returns a *full traceback*, which is useful for debugging but
  # never appropriate as a user-facing error. The design spec originally
  # called for py_last_error()$message; this probe locks in why we don't
  # use it. See I1 in 2026-04-21-query-v3-review.md and the corresponding
  # spec note in 2026-04-20-r-adapter-rewrite-design.md.
  py_err <- reticulate::py_last_error()
  if (!is.null(py_err) && !is.null(py_err$message)) {
    expect_true(
      startsWith(py_err$message, "Traceback") || identical(py_err$message, msg),
      info = sprintf(
        "py_last_error()$message format changed unexpectedly. First line: %s",
        substr(py_err$message, 1, 80)
      )
    )
  }
})
