test_that("drop_nulls removes NULL entries from a list", {
  expect_equal(drop_nulls(list(a = 1, b = NULL, c = "x")), list(a = 1, c = "x"))
  expect_equal(drop_nulls(list()), list())
  expect_equal(drop_nulls(list(a = NULL, b = NULL)), list())
})

test_that("drop_nulls preserves non-NULL falsy values", {
  expect_equal(drop_nulls(list(a = FALSE, b = 0, c = "", d = NA)),
               list(a = FALSE, b = 0, c = "", d = NA))
})

test_that("to_py_enum resolves a case-insensitive string against the enum's members", {
  fake_enum <- list(FILTER = "python_FILTER", SELECT = "python_SELECT")
  expect_equal(to_py_enum("FILTER", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"), "python_FILTER")
  expect_equal(to_py_enum("filter", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"), "python_FILTER")
  expect_equal(to_py_enum("Select", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"), "python_SELECT")
})

test_that("to_py_enum errors on unknown string with a helpful message listing valid values", {
  fake_enum <- list(FILTER = "x", SELECT = "y")
  err <- tryCatch(to_py_enum("REQUIRE", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
  expect_match(err$message, "FILTER", fixed = TRUE)
  expect_match(err$message, "SELECT", fixed = TRUE)
})

test_that("to_py_enum passes NULL through unchanged", {
  expect_null(to_py_enum(NULL, list(FILTER = "x"), "PhenotypicFilterType", "picsure_phenotypic_filter_type"))
})

# as_enum_string

test_that("as_enum_string returns NULL when value is NULL", {
  expect_null(picsure:::as_enum_string(NULL, "picsure_phenotypic_filter_type", "PhenotypicFilterType"))
})

test_that("as_enum_string returns the string itself for character input", {
  expect_equal(
    picsure:::as_enum_string("FILTER", "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    "FILTER"
  )
})

test_that("as_enum_string extracts the $name field by default for member input", {
  m <- picsure::PhenotypicFilterType$FILTER
  expect_equal(
    picsure:::as_enum_string(m, "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    "FILTER"
  )
})

test_that("as_enum_string extracts $value when field='value'", {
  m <- picsure::PhenotypicFilterType$FILTER
  expect_equal(
    picsure:::as_enum_string(m, "picsure_phenotypic_filter_type", "PhenotypicFilterType", field = "value"),
    "filter"
  )
})

test_that("as_enum_string rejects a member of the wrong subclass", {
  m <- picsure::GroupOperator$AND
  err <- tryCatch(
    picsure:::as_enum_string(m, "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
  expect_match(err$message, "GroupOperator", fixed = TRUE)
})

test_that("as_enum_string rejects non-string non-member input", {
  err <- tryCatch(
    picsure:::as_enum_string(42, "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
})

# to_py_enum extended

test_that("to_py_enum unwraps a member of the right subclass and resolves it via the proxy", {
  fake_enum <- list(FILTER = "python_FILTER", SELECT = "python_SELECT")
  m <- picsure::PhenotypicFilterType$FILTER
  expect_equal(
    to_py_enum(m, fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
    "python_FILTER"
  )
})

test_that("to_py_enum still accepts case-insensitive strings (backwards compat)", {
  fake_enum <- list(FILTER = "python_FILTER")
  expect_equal(
    to_py_enum("filter", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
    "python_FILTER"
  )
})

test_that("to_py_enum rejects a member of the wrong subclass before doing proxy lookup", {
  fake_enum <- list(FILTER = "python_FILTER")
  m <- picsure::GroupOperator$AND
  err <- tryCatch(
    to_py_enum(m, fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
  expect_match(err$message, "GroupOperator", fixed = TRUE)
})
