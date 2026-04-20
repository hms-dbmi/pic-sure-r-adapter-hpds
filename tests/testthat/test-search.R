test_that("search() delegates to session$search() and returns a data.frame", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  result <- picsure::search(bdc, "sex")

  expect_s3_class(result, "data.frame")
  expect_length(bdc$.calls$search, 1L)
  expect_equal(bdc$.calls$search[[1]][[1]], "sex")
})

test_that("search() forwards optional limit and offset as Python ints", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  picsure::search(bdc, "age", limit = 10, offset = 5)

  call <- bdc$.calls$search[[1]]
  expect_equal(call$keyword, "age")
  expect_identical(call$limit, 10L)
  expect_identical(call$offset, 5L)
})

test_that("search() omits NULL/NA optional kwargs so Python defaults fire", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  picsure::search(bdc, "age")  # no limit, no offset

  call <- bdc$.calls$search[[1]]
  expect_false("limit" %in% names(call))
  expect_false("offset" %in% names(call))
})

test_that("search() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$search <- function(...) {
    stop(structure(
      list(message = "The concept path 'sex' was not found in the dictionary."),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(picsure::search(bdc, "sex"), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not found in the dictionary", fixed = TRUE)
})
