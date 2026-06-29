test_that("connect() forwards platform and token to picsure_py$connect()", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  session <- picsure::connect(platform = "https://picsure.test", token = "abc123")

  expect_s3_class(session, "fake_session")
  expect_equal(session$platform, "https://picsure.test")
  expect_equal(session$token, "abc123")
  expect_length(fake$.calls$connect, 1L)
  expect_equal(fake$.calls$connect[[1]]$platform, "https://picsure.test")
  expect_equal(fake$.calls$connect[[1]]$token, "abc123")
})

test_that("connect() errors when platform is missing", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(picsure::connect(token = "abc"), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "platform")
})

test_that("connect() errors when an auth-required Platform member is given without a token", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::connect(platform = picsure::Platform$BDC_AUTHORIZED),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "token")
})

test_that("connect() defers token-presence check to Python for string platforms", {
  # We can't tell from R whether a custom URL requires auth, so an
  # empty/missing token is forwarded as "" and Python's
  # resolve_platform + validator decides.
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  picsure::connect(platform = "https://picsure.test")
  recorded <- fake$.calls$connect[[1]]
  expect_equal(recorded$token, "")
})

test_that("connect() accepts an open Platform member with no token", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  picsure::connect(platform = picsure::Platform$BDC_OPEN)
  recorded <- fake$.calls$connect[[1]]
  expect_equal(recorded$token, "")
})

test_that("connect() rejects NA platform and NA token", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bad_platforms <- list(NA, NA_character_)
  for (p in bad_platforms) {
    err <- tryCatch(picsure::connect(platform = p, token = "abc"), error = function(e) e)
    expect_s3_class(err, "picsureError")
    expect_match(conditionMessage(err), "platform")
  }
  bad_tokens <- list(NA, NA_character_)
  for (t in bad_tokens) {
    err <- tryCatch(picsure::connect(platform = "https://picsure.test", token = t), error = function(e) e)
    expect_s3_class(err, "picsureError")
    expect_match(conditionMessage(err), "token")
  }
})

test_that("connect() rejects length>1 token vectors as picsureError", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::connect(platform = "https://picsure.test", token = c("a", "b")),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "token")
})

test_that("connect() re-raises Python exceptions as picsureError", {
  failing_py <- fake_picsure_py()
  failing_py$connect <- function(platform, token, ...) {
    stop(structure(
      list(message = "Your token expired on 2026-03-14. Generate a new one."),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }
  testthat::local_mocked_bindings(picsure_py = failing_py)

  err <- tryCatch(
    picsure::connect(platform = "https://picsure.test", token = "stale"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "token expired", fixed = TRUE)
})

test_that("connect() rejects unknown extra kwargs with a helpful message", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::connect(platform = "https://picsure.test", token = "tok", resourceUuid = "x"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  msg <- conditionMessage(err)
  expect_match(msg, "resourceUuid", fixed = TRUE)
  expect_match(msg, "resource_uuid", fixed = TRUE)
  expect_match(msg, "include_consents", fixed = TRUE)
  expect_match(msg, "requires_auth", fixed = TRUE)
})

test_that("connect() forwards each whitelisted extra kwarg", {
  for (key in c("resource_uuid", "include_consents", "requires_auth")) {
    fake <- fake_picsure_py()
    testthat::local_mocked_bindings(picsure_py = fake)
    args <- list(platform = "https://picsure.test", token = "tok")
    args[[key]] <- "value-for-test"
    do.call(picsure::connect, args)
    expect_equal(
      fake$.calls$connect[[1]][[key]],
      "value-for-test",
      info = sprintf("kwarg %s should be forwarded", key)
    )
  }
})

test_that("connect() accepts a Platform member and forwards the matching Python enum", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  picsure::connect(platform = picsure::Platform$BDC_OPEN, token = "tok")

  recorded <- fake$.calls$connect[[1]]
  # The fake's Platform[[name]] returns the label string ("BDC Open")
  # for compatibility with the existing test fake. The real Python
  # adapter would return the actual Platform enum member here.
  expect_equal(recorded$platform, "BDC Open")
})

test_that("connect() rejects a non-Platform member with a clear error", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(
    picsure::connect(platform = picsure::PhenotypicFilterType$FILTER, token = "tok"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "Platform", fixed = TRUE)
})

test_that("connect() accepts a full URL string platform", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  picsure::connect(platform = "https://my-picsure.example.com", token = "tok")

  recorded <- fake$.calls$connect[[1]]
  expect_equal(recorded$platform, "https://my-picsure.example.com")
})

test_that("connect() rejects a human-readable label string before calling Python", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  for (label in c("BDC Authorized", "BDC Open", "Demo")) {
    err <- tryCatch(
      picsure::connect(platform = label, token = "tok"),
      error = function(e) e
    )
    expect_s3_class(err, "picsureError")
    expect_match(conditionMessage(err), "labels are not accepted", fixed = TRUE)
  }
  # Python is never reached for a rejected label.
  expect_length(fake$.calls$connect, 0L)
})

test_that("connect() defaults client_type to R_ADAPTER", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  picsure::connect(platform = "https://picsure.test", token = "abc")
  recorded <- fake$.calls$connect[[1]]
  expect_equal(recorded$client_type, "R_ADAPTER")
})

test_that("connect() lets the caller override client_type", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  picsure::connect(
    platform = "https://picsure.test", token = "abc",
    client_type = "PYTHON_ADAPTER"
  )
  recorded <- fake$.calls$connect[[1]]
  expect_equal(recorded$client_type, "PYTHON_ADAPTER")
})
