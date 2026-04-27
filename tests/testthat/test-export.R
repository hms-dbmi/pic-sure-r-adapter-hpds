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
  bdc$exportPFB <- function(query, path) {
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

test_that("exportCSV() forwards data.frame + path, returns path invisibly", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  df <- data.frame(id = 1:3, val = c("a", "b", "c"), stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".csv")

  result <- withVisible(picsure::exportCSV(bdc, df, tmp))

  expect_false(result$visible)
  expect_equal(result$value, tmp)
  expect_true(file.exists(tmp))
  expect_identical(bdc$.calls$exportCSV[[1]]$data, df)
  expect_equal(bdc$.calls$exportCSV[[1]]$path, tmp)
})

test_that("exportTSV() forwards data.frame + path, returns path invisibly", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  df <- data.frame(id = 1:3, val = c("a", "b", "c"), stringsAsFactors = FALSE)
  tmp <- tempfile(fileext = ".tsv")

  result <- withVisible(picsure::exportTSV(bdc, df, tmp))

  expect_false(result$visible)
  expect_equal(result$value, tmp)
  expect_true(file.exists(tmp))
  expect_identical(bdc$.calls$exportTSV[[1]]$data, df)
  expect_equal(bdc$.calls$exportTSV[[1]]$path, tmp)
})

test_that("exportCSV() errors when data is not a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  tmp <- tempfile(fileext = ".csv")

  expect_error(picsure::exportCSV(bdc, list(a = 1), tmp), "data.frame")
  expect_error(picsure::exportCSV(bdc, NULL, tmp), "data.frame")
})

test_that("exportTSV() errors on missing path", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  df <- data.frame(id = 1:2)
  expect_error(picsure::exportTSV(bdc, df), "path")
})

test_that("exportCSV() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$exportCSV <- function(data, path) {
    stop(structure(
      list(message = "permission denied on path"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }
  df <- data.frame(id = 1:2)

  err <- tryCatch(
    picsure::exportCSV(bdc, df, tempfile()),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "permission denied", fixed = TRUE)
})

test_that("exportCSV() errors on missing path", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  df <- data.frame(id = 1:2)
  expect_error(picsure::exportCSV(bdc, df), "path")
})

test_that("exportTSV() errors when data is not a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  tmp <- tempfile(fileext = ".tsv")
  expect_error(picsure::exportTSV(bdc, list(a = 1), tmp), "data.frame")
  expect_error(picsure::exportTSV(bdc, NULL, tmp), "data.frame")
})
