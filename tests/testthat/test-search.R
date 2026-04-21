test_that("search() delegates to session$search() and returns a data.frame", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  result <- picsure::search(bdc, "sex")

  expect_s3_class(result, "data.frame")
  expect_length(bdc$.calls$search, 1L)
  expect_equal(bdc$.calls$search[[1]]$term, "sex")
})

test_that("search() forwards include_values to Python", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  picsure::search(bdc, "age", include_values = FALSE)

  call <- bdc$.calls$search[[1]]
  expect_equal(call$term, "age")
  expect_false(call$include_values)
})

test_that("search() defaults term to empty string (all variables)", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  picsure::search(bdc)

  call <- bdc$.calls$search[[1]]
  expect_equal(call$term, "")
})

test_that("search() rejects non-string term (NULL, NA, numeric, multi-length)", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  expect_error(picsure::search(bdc, NULL), "term")
  expect_error(picsure::search(bdc, NA_character_), "term")
  expect_error(picsure::search(bdc, 42), "term")
  expect_error(picsure::search(bdc, c("a", "b")), "term")
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

test_that("search() forwards a FacetSet as the facets kwarg", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "data_source", "topmed")

  picsure::search(bdc, "sex", facets = fs)

  call <- bdc$.calls$search[[1]]
  expect_identical(call$facets, fs)
})
