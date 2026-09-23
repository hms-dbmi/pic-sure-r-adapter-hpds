test_that("searchDictionary() delegates to session$searchDictionary() and returns a data.frame", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  result <- picsure::searchDictionary(bdc, "sex")

  expect_s3_class(result, "data.frame")
  expect_length(bdc$.calls$searchDictionary, 1L)
  expect_equal(bdc$.calls$searchDictionary[[1]]$term, "sex")
})

test_that("searchDictionary() forwards include_values to Python", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  picsure::searchDictionary(bdc, "age", include_values = FALSE)

  call <- bdc$.calls$searchDictionary[[1]]
  expect_equal(call$term, "age")
  expect_false(call$include_values)
})

test_that("searchDictionary() defaults term to empty string (all variables)", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  picsure::searchDictionary(bdc)

  call <- bdc$.calls$searchDictionary[[1]]
  expect_equal(call$term, "")
})

test_that("searchDictionary() rejects non-string term (NULL, NA, numeric, multi-length)", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  expect_error(picsure::searchDictionary(bdc, NULL), "term")
  expect_error(picsure::searchDictionary(bdc, NA_character_), "term")
  expect_error(picsure::searchDictionary(bdc, 42), "term")
  expect_error(picsure::searchDictionary(bdc, c("a", "b")), "term")
})

test_that("searchDictionary() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  bdc$searchDictionary <- function(...) {
    stop(structure(
      list(message = "The concept path 'sex' was not found in the dictionary."),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(picsure::searchDictionary(bdc, "sex"), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not found in the dictionary", fixed = TRUE)
})

test_that("searchDictionary() forwards a FacetSet as the facets kwarg", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "data_source", "topmed")

  picsure::searchDictionary(bdc, "sex", facets = fs)

  call <- bdc$.calls$searchDictionary[[1]]
  expect_identical(call$facets, fs)
})

test_that("searchDictionary() rejects a non-string term as a picsureValidationError", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  for (term in list(NULL, NA_character_, 42, c("a", "b"), character(0), list("a"))) {
    err <- tryCatch(picsure::searchDictionary(bdc, term), condition = function(e) e)
    expect_s3_class(err, "picsureValidationError")
    expect_s3_class(err, "picsureError")
    expect_match(conditionMessage(err), "`term`", fixed = TRUE)
  }
  expect_length(bdc$.calls$searchDictionary, 0L)
})

test_that("searchDictionary() types the columns it knows and leaves the rest", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  bdc$searchDictionary <- function(...) {
    data.frame(
      conceptPath = "\\phs1\\bmi\\", min = "12.5", max = "61", allowFiltering = "TRUE",
      unknownField = "left alone", stringsAsFactors = FALSE
    )
  }

  result <- picsure::searchDictionary(bdc, "bmi")

  expect_type(result$min, "double")
  expect_type(result$max, "double")
  expect_type(result$allowFiltering, "logical")
  expect_type(result$unknownField, "character")
  expect_identical(names(result), c("conceptPath", "min", "max", "allowFiltering", "unknownField"))
})

test_that("searchDictionary() sends no page or page_size unless given", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  picsure::searchDictionary(bdc, "age")

  call <- bdc$.calls$searchDictionary[[1]]
  expect_false("page" %in% names(call))
  expect_false("page_size" %in% names(call))
})

test_that("searchDictionary() forwards page and page_size as integers", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  picsure::searchDictionary(bdc, "", page = 0, page_size = 100)
  picsure::searchDictionary(bdc, "", page = 3L, page_size = 2.0)

  first <- bdc$.calls$searchDictionary[[1]]
  expect_identical(first$page, 0L)
  expect_identical(first$page_size, 100L)
  second <- bdc$.calls$searchDictionary[[2]]
  expect_identical(second$page, 3L)
  expect_identical(second$page_size, 2L)
})

test_that("searchDictionary() rejects a page that is not a whole number, 0 or greater", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  for (value in list(-1, 1.5, "0", NA_real_, Inf, c(0, 1), TRUE)) {
    label <- paste(deparse(value), collapse = "")
    err <- tryCatch(picsure::searchDictionary(bdc, "", page = value), error = function(e) e)
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`page`", fixed = TRUE, info = label)
    expect_identical(deparse(conditionCall(err)[[1L]]), "picsure::searchDictionary",
                     info = label)
  }
  expect_length(bdc$.calls$searchDictionary, 0L)
})

test_that("searchDictionary() rejects a page_size that is not a positive whole number", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  for (value in list(0, -10, 2.5, "100", NA_integer_, 3e9, c(10, 20))) {
    label <- paste(deparse(value), collapse = "")
    err <- tryCatch(picsure::searchDictionary(bdc, "", page_size = value), error = function(e) e)
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`page_size`", fixed = TRUE, info = label)
  }
  expect_length(bdc$.calls$searchDictionary, 0L)
})
