test_that("facets() returns a FacetSet handle from the session", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  fs <- picsure::facets(bdc)

  expect_s3_class(fs, "fake_facet_set")
})

test_that("facets() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$facets <- function() {
    stop(structure(
      list(message = "facets endpoint unavailable"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(picsure::facets(bdc), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "facets endpoint", fixed = TRUE)
})

test_that("addFacet() delegates to fs$add() and returns the FacetSet", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  fs <- picsure::facets(bdc)

  result <- picsure::addFacet(fs, "study_ids", "phs000007")

  expect_identical(result, fs)
  expect_length(fs$.state$entries, 1L)
  expect_equal(fs$.state$entries[[1]]$key,   "study_ids")
  expect_equal(fs$.state$entries[[1]]$value, "phs000007")
})

test_that("addFacet() supports vector values by calling fs$add() once per value", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  fs <- picsure::facets(bdc)

  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200"))

  expect_length(fs$.state$entries, 2L)
  expect_equal(fs$.state$entries[[1]]$value, "phs000007")
  expect_equal(fs$.state$entries[[2]]$value, "phs000200")
})

test_that("removeFacet() delegates to fs$remove() and returns the FacetSet", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "study_ids", "phs000007")
  picsure::addFacet(fs, "study_ids", "phs000200")

  result <- picsure::removeFacet(fs, "study_ids", "phs000007")

  expect_identical(result, fs)
  expect_length(fs$.state$entries, 1L)
  expect_equal(fs$.state$entries[[1]]$value, "phs000200")
})

test_that("addFacet() re-raises Python exceptions as picsureError", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  fs <- picsure::facets(bdc)
  fs$add <- function(key, value) {
    stop(structure(
      list(message = "invalid facet key 'xyz'"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(
    picsure::addFacet(fs, "xyz", "v"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "invalid facet key", fixed = TRUE)
})
