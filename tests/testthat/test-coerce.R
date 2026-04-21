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
  expect_equal(to_py_enum("FILTER", fake_enum, "ClauseType"), "python_FILTER")
  expect_equal(to_py_enum("filter", fake_enum, "ClauseType"), "python_FILTER")
  expect_equal(to_py_enum("Select", fake_enum, "ClauseType"), "python_SELECT")
})

test_that("to_py_enum errors on unknown string with a helpful message listing valid values", {
  fake_enum <- list(FILTER = "x", SELECT = "y")
  err <- tryCatch(to_py_enum("REQUIRE", fake_enum, "ClauseType"),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(err$message, "ClauseType", fixed = TRUE)
  expect_match(err$message, "FILTER", fixed = TRUE)
  expect_match(err$message, "SELECT", fixed = TRUE)
})

test_that("to_py_enum passes NULL through unchanged", {
  expect_null(to_py_enum(NULL, list(FILTER = "x"), "ClauseType"))
})
