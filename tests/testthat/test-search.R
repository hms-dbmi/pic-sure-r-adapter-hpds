test_that("searchDictionary() delegates to session$searchDictionary() and returns a data.frame", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  result <- picsure::searchDictionary(bdc, "sex")

  expect_s3_class(result, "data.frame")
  expect_length(bdc$.calls$searchDictionary, 1L)
  expect_equal(bdc$.calls$searchDictionary[[1]]$term, "sex")
})

test_that("searchDictionary() forwards include_values to Python", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  picsure::searchDictionary(bdc, "age", include_values = FALSE)

  call <- bdc$.calls$searchDictionary[[1]]
  expect_equal(call$term, "age")
  expect_false(call$include_values)
})

test_that("searchDictionary() defaults term to empty string (all variables)", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  picsure::searchDictionary(bdc)

  call <- bdc$.calls$searchDictionary[[1]]
  expect_equal(call$term, "")
})

test_that("searchDictionary() rejects non-string term (NULL, NA, numeric, multi-length)", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  expect_error(picsure::searchDictionary(bdc, NULL), "term")
  expect_error(picsure::searchDictionary(bdc, NA_character_), "term")
  expect_error(picsure::searchDictionary(bdc, 42), "term")
  expect_error(picsure::searchDictionary(bdc, c("a", "b")), "term")
})

test_that("searchDictionary() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
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
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "data_source", "topmed")

  picsure::searchDictionary(bdc, "sex", facets = fs)

  call <- bdc$.calls$searchDictionary[[1]]
  expect_identical(call$facets, fs)
})
