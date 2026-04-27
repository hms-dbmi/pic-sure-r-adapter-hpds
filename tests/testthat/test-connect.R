test_that("connect() forwards platform and token to picsure_py$connect()", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  session <- picsure::connect(platform = "BDC Authorized", token = "abc123")

  expect_s3_class(session, "fake_session")
  expect_equal(session$platform, "BDC Authorized")
  expect_equal(session$token, "abc123")
  expect_length(fake$.calls$connect, 1L)
  expect_equal(fake$.calls$connect[[1]]$platform, "BDC Authorized")
  expect_equal(fake$.calls$connect[[1]]$token, "abc123")
})

test_that("connect() errors when platform is missing", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::connect(token = "abc"), "platform")
})

test_that("connect() errors when token is missing", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::connect(platform = "Demo"), "token")
})

test_that("connect() rejects NA platform and NA token", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::connect(platform = NA, token = "abc"), "platform")
  expect_error(picsure::connect(platform = NA_character_, token = "abc"), "platform")
  expect_error(picsure::connect(platform = "Demo", token = NA), "token")
  expect_error(picsure::connect(platform = "Demo", token = NA_character_), "token")
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
    picsure::connect(platform = "Demo", token = "stale"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "token expired", fixed = TRUE)
})

test_that("connect() rejects unknown extra kwargs with a helpful message", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::connect(platform = "Demo", token = "tok", resourceUuid = "x"),
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
    args <- list(platform = "Demo", token = "tok")
    args[[key]] <- "value-for-test"
    do.call(picsure::connect, args)
    expect_equal(
      fake$.calls$connect[[1]][[key]],
      "value-for-test",
      info = sprintf("kwarg %s should be forwarded", key)
    )
  }
})
