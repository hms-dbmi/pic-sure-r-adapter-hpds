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

test_that("a missing token on an auth-required Platform member is a picsureValidationError", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  for (args in list(
    list(platform = picsure::Platform$BDC_AUTHORIZED),
    list(platform = picsure::Platform$BDC_AUTHORIZED, token = "")
  )) {
    err <- tryCatch(do.call(picsure::connect, args), error = function(e) e)
    expect_s3_class(err, "picsureValidationError")
    expect_false(inherits(err, "picsureAuthError"))
    expect_match(conditionMessage(err), "token")
  }
})

test_that("requires_auth = FALSE on an auth-required member does not demand a token", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  picsure::connect(
    platform = picsure::Platform$BDC_AUTHORIZED,
    requires_auth = FALSE,
    include_consents = FALSE
  )
  recorded <- fake$.calls$connect[[1]]
  expect_identical(recorded$token, "")
  expect_identical(recorded$requires_auth, FALSE)
  expect_identical(recorded$include_consents, FALSE)
})

test_that("requires_auth = TRUE on a URL string demands a token in R", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::connect(platform = "https://picsure.test", requires_auth = TRUE),
    error = function(e) e
  )
  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "`token` is required", fixed = TRUE)
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
  expect_match(msg, "include_consents", fixed = TRUE)
  expect_match(msg, "requires_auth", fixed = TRUE)
  expect_match(msg, "supports_genomic", fixed = TRUE)
})

test_that("connect() forwards timeout to Python as a double", {
  for (value in list(60, 60L, 0.5)) {
    fake <- fake_picsure_py()
    testthat::local_mocked_bindings(picsure_py = fake)
    picsure::connect(platform = "https://picsure.test", token = "tok", timeout = value)
    forwarded <- fake$.calls$connect[[1]]$timeout
    expect_type(forwarded, "double")
    expect_identical(forwarded, as.double(value))
  }
})

test_that("connect() sends no timeout when none is given, so Python's default stands", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  picsure::connect(platform = "https://picsure.test", token = "tok")
  recorded <- fake$.calls$connect[[1]]
  expect_false("timeout" %in% names(recorded))
  expect_false("validate" %in% names(recorded))
})

test_that("connect() rejects a timeout that is not a single positive finite number", {
  for (value in list("abc", "60", -5, 0, NA_real_, NaN, Inf, c(10, 20), TRUE, list(60))) {
    fake <- fake_picsure_py()
    testthat::local_mocked_bindings(picsure_py = fake)
    err <- tryCatch(
      picsure::connect(platform = "https://picsure.test", token = "tok", timeout = value),
      error = function(e) e
    )
    label <- paste(deparse(value), collapse = "")
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`timeout`", fixed = TRUE, info = label)
    expect_identical(deparse(conditionCall(err)[[1L]]), "picsure::connect", info = label)
    expect_length(fake$.calls$connect, 0L)
  }
})

test_that("connect() forwards validate as a logical", {
  for (value in c(TRUE, FALSE)) {
    fake <- fake_picsure_py()
    testthat::local_mocked_bindings(picsure_py = fake)
    picsure::connect(platform = "https://picsure.test", token = "tok", validate = value)
    expect_identical(fake$.calls$connect[[1]]$validate, value)
  }
})

test_that("connect() rejects a validate that is not a single TRUE or FALSE", {
  for (value in list("FALSE", "false", 0, 1L, NA, c(TRUE, FALSE), logical(0))) {
    fake <- fake_picsure_py()
    testthat::local_mocked_bindings(picsure_py = fake)
    err <- tryCatch(
      picsure::connect(platform = "https://picsure.test", token = "tok", validate = value),
      error = function(e) e
    )
    label <- paste(deparse(value), collapse = "")
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`validate` must be TRUE or FALSE",
                 fixed = TRUE, info = label)
    expect_length(fake$.calls$connect, 0L)
  }
})

test_that("connect() rejects resource_uuid, which no longer routes anything", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::connect(
      platform = "https://picsure.test", token = "tok", resource_uuid = "x"
    ),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "Unknown argument", fixed = TRUE)
})

test_that("connect() forwards each whitelisted extra kwarg", {
  values <- list(
    include_consents = "value-for-test",
    requires_auth    = "value-for-test",
    supports_genomic = "value-for-test",
    timeout          = 42,
    validate         = FALSE
  )
  for (key in names(values)) {
    fake <- fake_picsure_py()
    testthat::local_mocked_bindings(picsure_py = fake)
    args <- list(platform = "https://picsure.test", token = "tok")
    args[[key]] <- values[[key]]
    do.call(picsure::connect, args)
    expect_equal(
      fake$.calls$connect[[1]][[key]],
      values[[key]],
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

test_that("connect() rejects a Platform member the Python enum does not define", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  unknown <- picsure::Platform$BDC_OPEN
  unknown$name <- "NO_SUCH_MEMBER"

  err <- tryCatch(
    picsure::connect(platform = unknown, token = "tok"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureValidationError")
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "NO_SUCH_MEMBER", fixed = TRUE)
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

test_that("connect() forwards options(picsure.ssl_verify) as the verify kwarg", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  with_picsure_options(list(picsure.ssl_verify = FALSE), {
    picsure::connect(platform = "https://picsure.test", token = "tok")
  })

  expect_identical(fake$.calls$connect[[1]]$verify, FALSE)
})

test_that("connect() forwards a CA-bundle path from options(picsure.ssl_verify)", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  with_picsure_options(list(picsure.ssl_verify = "/etc/ssl/my-ca.pem"), {
    picsure::connect(platform = "https://picsure.test", token = "tok")
  })

  expect_identical(fake$.calls$connect[[1]]$verify, "/etc/ssl/my-ca.pem")
})

test_that("connect() forwards options(picsure.dev_mode) as the dev_mode kwarg", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  with_picsure_options(list(picsure.dev_mode = TRUE), {
    picsure::connect(platform = "https://picsure.test", token = "tok")
  })

  expect_identical(fake$.calls$connect[[1]]$dev_mode, TRUE)
})

test_that("an explicit verify / dev_mode argument beats the option", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  with_picsure_options(list(picsure.ssl_verify = FALSE, picsure.dev_mode = TRUE), {
    picsure::connect(platform = "https://picsure.test", token = "tok",
                     verify = TRUE, dev_mode = FALSE)
  })

  recorded <- fake$.calls$connect[[1]]
  expect_identical(recorded$verify, TRUE)
  expect_identical(recorded$dev_mode, FALSE)
})

test_that("neither kwarg is sent when the option is unset, so Python's default stands", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  with_picsure_options(list(picsure.ssl_verify = NULL, picsure.dev_mode = NULL), {
    picsure::connect(platform = "https://picsure.test", token = "tok")
  })

  recorded <- fake$.calls$connect[[1]]
  expect_false("verify" %in% names(recorded))
  expect_false("dev_mode" %in% names(recorded))
})

test_that("the options are read on every call, not cached", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  with_picsure_options(list(picsure.ssl_verify = FALSE), {
    picsure::connect(platform = "https://picsure.test", token = "tok")
  })
  with_picsure_options(list(picsure.ssl_verify = TRUE), {
    picsure::connect(platform = "https://picsure.test", token = "tok")
  })

  expect_identical(fake$.calls$connect[[1]]$verify, FALSE)
  expect_identical(fake$.calls$connect[[2]]$verify, TRUE)
})

test_that("an unusable option value is a picsureError naming the option", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())

  bad_verify <- list(NA, "", 1, c(TRUE, FALSE), c("a", "b"), list(TRUE))
  for (value in bad_verify) {
    err <- with_picsure_options(
      list(picsure.ssl_verify = value),
      tryCatch(picsure::connect(platform = "https://picsure.test", token = "tok"),
               condition = function(e) e)
    )
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "picsure.ssl_verify", fixed = TRUE)
  }

  for (value in list(NA, "TRUE", 1, c(TRUE, FALSE))) {
    err <- with_picsure_options(
      list(picsure.dev_mode = value),
      tryCatch(picsure::connect(platform = "https://picsure.test", token = "tok"),
               condition = function(e) e)
    )
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "picsure.dev_mode", fixed = TRUE)
  }
})

test_that("an explicit verify or dev_mode argument is validated like the option", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())

  err <- tryCatch(
    picsure::connect(platform = "https://picsure.test", token = "tok", dev_mode = "FALSE"),
    condition = function(e) e
  )
  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "`dev_mode` must be TRUE or FALSE", fixed = TRUE)

  for (value in list(NA, "", 1, c(TRUE, FALSE))) {
    err <- tryCatch(
      picsure::connect(platform = "https://picsure.test", token = "tok", verify = value),
      condition = function(e) e
    )
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`verify` must be TRUE, FALSE, or a path to a CA bundle",
                 fixed = TRUE)
  }
})

test_that("a string that spells a boolean is rejected for verify, argument or option", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())

  for (value in c("false", "FALSE", "0", "no", "off")) {
    err <- tryCatch(
      picsure::connect(platform = "https://picsure.test", token = "tok", verify = value),
      condition = function(e) e
    )
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "TRUE, FALSE, or a path to a CA bundle", fixed = TRUE)
    expect_match(conditionMessage(err), "Pass the logical FALSE instead", fixed = TRUE)
  }
  for (value in c("true", "1", "yes", "on")) {
    err <- tryCatch(
      picsure::connect(platform = "https://picsure.test", token = "tok", verify = value),
      condition = function(e) e
    )
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "Pass the logical TRUE instead", fixed = TRUE)
  }

  err <- with_picsure_options(
    list(picsure.ssl_verify = "false"),
    tryCatch(picsure::connect(platform = "https://picsure.test", token = "tok"),
             condition = function(e) e)
  )
  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "options(picsure.ssl_verify) must be TRUE, FALSE, or a path",
               fixed = TRUE)
})

test_that("a CA bundle path passed as the verify argument is forwarded unchanged", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  expect_no_message(
    picsure::connect(platform = "https://picsure.test", token = "tok", verify = "/etc/ssl/my-ca.pem")
  )
  expect_identical(fake$.calls$connect[[1]]$verify, "/etc/ssl/my-ca.pem")
})

test_that("connect() says when TLS verification is off and names what turned it off", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  expect_message(
    picsure::connect(platform = "https://picsure.test", token = "tok", verify = FALSE),
    "TLS certificate verification is off.*the `verify` argument"
  )
  expect_message(
    with_picsure_options(list(picsure.ssl_verify = FALSE), {
      picsure::connect(platform = "https://picsure.test", token = "tok")
    }),
    "TLS certificate verification is off.*options\\(picsure.ssl_verify\\)"
  )
  expect_identical(fake$.calls$connect[[1]]$verify, FALSE)
  expect_identical(fake$.calls$connect[[2]]$verify, FALSE)
})

test_that("connect() is quiet when TLS verification is on or unset", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  expect_no_message(
    picsure::connect(platform = "https://picsure.test", token = "tok", verify = TRUE)
  )
  expect_no_message(
    with_picsure_options(list(picsure.ssl_verify = NULL), {
      picsure::connect(platform = "https://picsure.test", token = "tok")
    })
  )
  expect_no_message(
    with_picsure_options(list(picsure.ssl_verify = TRUE), {
      picsure::connect(platform = "https://picsure.test", token = "tok")
    })
  )
})
