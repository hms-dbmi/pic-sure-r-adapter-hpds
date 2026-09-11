test_that("facets() returns a FacetSet handle from the session", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  fs <- picsure::facets(bdc)

  expect_s3_class(fs, "fake_facet_set")
})

test_that("facets() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
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
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)

  result <- picsure::addFacet(fs, "study_ids", "phs000007")

  expect_identical(result, fs)
  expect_length(fs$.state$entries, 1L)
  expect_equal(fs$.state$entries[[1]]$key,   "study_ids")
  expect_equal(fs$.state$entries[[1]]$value, "phs000007")
})

test_that("addFacet() supports vector values by calling fs$add() once per value", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)

  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200"))

  expect_length(fs$.state$entries, 2L)
  expect_equal(fs$.state$entries[[1]]$value, "phs000007")
  expect_equal(fs$.state$entries[[2]]$value, "phs000200")
})

test_that("removeFacet() rewrites the category without the value", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "study_ids", "phs000007")
  picsure::addFacet(fs, "study_ids", "phs000200")

  result <- picsure::removeFacet(fs, "study_ids", "phs000007")

  expect_identical(result, fs)
  expect_length(fs$.state$entries, 1L)
  expect_equal(fs$.state$entries[[1]]$value, "phs000200")
})

test_that("removeFacet() leaves other categories alone", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "study_ids", "phs000007")
  picsure::addFacet(fs, "data_source", "topmed")

  picsure::removeFacet(fs, "study_ids", "phs000007")

  expect_equal(fs$view()[["study_ids"]], character(0))
  expect_equal(fs$view()[["data_source"]], "topmed")
})

test_that("removeFacet() is a no-op for a value that was never added", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200"))

  picsure::removeFacet(fs, "study_ids", "phs999999")

  expect_equal(fs$view()[["study_ids"]], c("phs000007", "phs000200"))
})

test_that("addFacet() re-raises Python exceptions as picsureError", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
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

test_that("removeFacet() re-raises Python exceptions as picsureError", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)
  fs$clear <- function(category = NULL) {
    stop(structure(
      list(message = "'xyz' is not a valid facet category."),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(
    picsure::removeFacet(fs, "xyz", "phs000007"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not a valid facet category", fixed = TRUE)
})

# RR-11: a length > 1 key is a package error, not a base R condition-length error

test_that("addFacet() rejects multiple keys with a package error", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)

  err <- tryCatch(
    picsure::addFacet(fs, c("study_ids", "data_source"), "phs000007"),
    condition = function(e) e
  )

  expect_s3_class(err, "picsureValidationError")
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "`key`", fixed = TRUE)
  expect_match(conditionMessage(err), "2 values", fixed = TRUE)
  expect_match(conditionMessage(err), "one key", fixed = TRUE)
  # The base R complaint this replaces.
  expect_false(grepl("coercion to 'logical(1)'", conditionMessage(err), fixed = TRUE))
  expect_length(fs$.state$entries, 0L)
})

test_that("removeFacet() rejects multiple keys with a package error", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)
  picsure::addFacet(fs, "study_ids", "phs000007")

  err <- tryCatch(
    picsure::removeFacet(fs, c("study_ids", "data_source"), "phs000007"),
    condition = function(e) e
  )

  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "`key`", fixed = TRUE)
  # The FacetSet is untouched: the key is checked before anything is cleared.
  expect_equal(fs$view()[["study_ids"]], "phs000007")
})

test_that("both facet wrappers reject every non-scalar-string key", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)

  for (key in list(NULL, NA_character_, NA, "", character(0), 42,
                   c("study_ids", "data_source"), list("study_ids"))) {
    for (call in list(picsure::addFacet, picsure::removeFacet)) {
      err <- tryCatch(call(fs, key, "v"), condition = function(e) e)
      expect_s3_class(err, "picsureValidationError")
      expect_match(conditionMessage(err), "`key`", fixed = TRUE)
    }
  }
})

test_that("multiple facet values stay legitimate", {
  # The Python FacetSet's add(category, values) takes one category and a
  # vector of values, so the vector belongs on `value`, never on `key`.
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)

  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200", "phs000286"))
  expect_equal(fs$view()[["study_ids"]], c("phs000007", "phs000200", "phs000286"))

  picsure::removeFacet(fs, "study_ids", c("phs000007", "phs000286"))
  expect_equal(fs$view()[["study_ids"]], "phs000200")
})

test_that("a missing or empty value is a package error", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  fs <- picsure::facets(bdc)

  for (call in list(picsure::addFacet, picsure::removeFacet)) {
    for (value in list(NULL, character(0))) {
      err <- tryCatch(call(fs, "study_ids", value), condition = function(e) e)
      expect_s3_class(err, "picsureValidationError")
      expect_match(conditionMessage(err), "`value`", fixed = TRUE)
    }
    err <- tryCatch(call(fs, "study_ids"), condition = function(e) e)
    expect_s3_class(err, "picsureValidationError")
  }
})
