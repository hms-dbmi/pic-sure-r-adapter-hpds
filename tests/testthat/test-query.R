test_that("runQuery() with type='count' returns an integer scalar", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  result <- picsure::runQuery(bdc, q, type = "count")

  expect_type(result, "integer")
  expect_equal(result, 42L)
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

  expect_equal(r1, 42L)
  expect_equal(r2, 42L)
  expect_equal(r3, 42L)
})

test_that("runQuery() defaults type to 'count'", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  result <- picsure::runQuery(bdc, q)

  expect_equal(result, 42L)
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
