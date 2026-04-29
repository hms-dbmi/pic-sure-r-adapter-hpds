test_that("runQuery() with type='count' returns a CountResult-shaped object", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  result <- picsure::runQuery(bdc, q, type = "count")

  expect_true(!is.null(result$value))
  expect_equal(result$value, 42L)
})

test_that("runQuery() with type='participant' returns a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  result <- picsure::runQuery(bdc, q, type = "participant")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("patient_id" %in% names(result))
})

test_that("runQuery() accepts case-insensitive type", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  r1 <- picsure::runQuery(bdc, q, type = "COUNT")
  r2 <- picsure::runQuery(bdc, q, type = "Count")
  r3 <- picsure::runQuery(bdc, q, type = "count")

  expect_equal(r1$value, 42L)
  expect_equal(r2$value, 42L)
  expect_equal(r3$value, 42L)
})

test_that("runQuery() defaults type to 'count'", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  result <- picsure::runQuery(bdc, q)

  expect_equal(result$value, 42L)
})

test_that("runQuery() forwards query and type to session$runQuery", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  picsure::runQuery(bdc, q, type = "participant")

  call <- bdc$.calls$runQuery[[1]]
  expect_identical(call$query, q)
  expect_equal(tolower(call$type), "participant")
})

test_that("runQuery() errors on missing query", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  expect_error(picsure::runQuery(bdc), "query")
})

test_that("runQuery() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$runQuery <- function(...) {
    stop(structure(
      list(message = "query rejected: no SELECT clauses"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(
    picsure::runQuery(bdc, list(kind = "group", clauses = list(), root = "AND")),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "no SELECT clauses", fixed = TRUE)
})

test_that("runQuery() with type='timestamp' returns a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  result <- picsure::runQuery(bdc, q, type = "timestamp")

  expect_s3_class(result, "data.frame")
  expect_true("timestamp" %in% names(result))
})

test_that("runQuery() forwards an unknown type to Python for the error", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  err <- tryCatch(
    picsure::runQuery(bdc, q, type = "nonsense"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
})

test_that("runQuery() accepts a QueryType member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  result <- picsure::runQuery(session, query = list(kind = "group"),
                              type = picsure::QueryType$COUNT)

  recorded <- session$.calls$runQuery[[1]]
  expect_equal(recorded$type, "count")  # fake's QueryType$COUNT value
  expect_equal(result$value, 42L)
})

test_that("runQuery() rejects a wrong-subclass member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  err <- tryCatch(
    picsure::runQuery(session, query = list(kind = "group"),
                      type = picsure::ClauseType$FILTER),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "QueryType", fixed = TRUE)
})

test_that("runQuery() still accepts case-insensitive strings (backwards compat)", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  picsure::runQuery(session, query = list(kind = "group"), type = "PARTICIPANT")

  recorded <- session$.calls$runQuery[[1]]
  # to_py_enum's case-insensitive lookup resolves "PARTICIPANT" against
  # the fake's PARTICIPANT key and returns the fake's stored lowercase
  # value "participant". Pin the exact case so a future refactor that
  # forwards the raw input would fail this assertion.
  expect_equal(recorded$type, "participant")
})
