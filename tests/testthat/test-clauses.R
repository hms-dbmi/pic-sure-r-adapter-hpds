test_that("createClause() delegates to picsure_py$createClause() with type resolved via to_py_enum", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::createClause("\\phs1\\pht1\\phv1\\sex\\", type = "FILTER",
                                  categories = list("male"))

  expect_equal(clause$kind, "clause")
  expect_equal(clause$path, "\\phs1\\pht1\\phv1\\sex\\")
  expect_equal(clause$type, "FILTER")  # fake's ClauseType$FILTER value
  expect_equal(clause$extra$categories, list("male"))
})

test_that("createClause() accepts case-insensitive type strings", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  c1 <- picsure::createClause("x", type = "filter")
  c2 <- picsure::createClause("x", type = "Filter")
  c3 <- picsure::createClause("x", type = "FILTER")

  expect_equal(c1$type, "FILTER")
  expect_equal(c2$type, "FILTER")
  expect_equal(c3$type, "FILTER")
})

test_that("createClause() errors on unknown type string", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::createClause("x", type = "BOGUS"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(err$message, "BOGUS", fixed = TRUE)
  expect_match(err$message, "FILTER", fixed = TRUE)
})

test_that("createClause() coerces min and max to Python ints when integral", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::createClause("x", type = "FILTER", min = 40L, max = 80L)

  expect_identical(clause$extra$min, 40L)
  expect_identical(clause$extra$max, 80L)
})

test_that("createClause() drops NULL optional args so Python defaults fire", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::createClause("x", type = "FILTER")

  expect_false("min" %in% names(clause$extra))
  expect_false("max" %in% names(clause$extra))
  expect_false("categories" %in% names(clause$extra))
})

test_that("createClause() errors on missing path", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::createClause(type = "FILTER"), "path")
})

test_that("createClause() errors on missing type", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::createClause("x"), "type")
})

test_that("createClause() re-raises Python exceptions as picsureError", {
  failing <- fake_picsure_py()
  failing$createClause <- function(path, type, ...) {
    stop(structure(
      list(message = "concept path 'x' not found in dictionary"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }
  testthat::local_mocked_bindings(picsure_py = failing)

  err <- tryCatch(
    picsure::createClause("x", type = "FILTER"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not found in dictionary", fixed = TRUE)
})
