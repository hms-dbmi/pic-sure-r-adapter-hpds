test_that("exportPFB() forwards query and path to session$exportPFB", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")
  tmp <- tempfile(fileext = ".pfb")

  result <- picsure::exportPFB(bdc, q, tmp)

  expect_equal(result, tmp)
  expect_true(file.exists(tmp))
  call <- bdc$.calls$exportPFB[[1]]
  expect_identical(call$query, q)
  expect_equal(call$path, tmp)
})

test_that("exportPFB() returns the path invisibly", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")
  tmp <- tempfile(fileext = ".pfb")

  result <- withVisible(picsure::exportPFB(bdc, q, tmp))
  expect_false(result$visible)
  expect_equal(result$value, tmp)
})

test_that("exportPFB() errors on missing path", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), root = "AND")

  expect_error(picsure::exportPFB(bdc, q), "path")
})

test_that("exportPFB() errors on missing query", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  tmp <- tempfile(fileext = ".pfb")

  expect_error(picsure::exportPFB(bdc, path = tmp), "query")
})

test_that("exportPFB() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$exportPFB <- function(query, path, ...) {
    stop(structure(
      list(message = "PFB export requires the picsure[pfb] optional dependency"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(
    picsure::exportPFB(bdc, list(kind = "group"), tempfile()),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "optional dependency", fixed = TRUE)
})
