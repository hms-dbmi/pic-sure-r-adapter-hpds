test_that("drop_nulls removes NULL entries from a list", {
  expect_equal(drop_nulls(list(a = 1, b = NULL, c = "x")), list(a = 1, c = "x"))
  expect_equal(drop_nulls(list()), list())
  expect_equal(drop_nulls(list(a = NULL, b = NULL)), list())
})

test_that("drop_nulls preserves non-NULL falsy values", {
  expect_equal(drop_nulls(list(a = FALSE, b = 0, c = "", d = NA)),
               list(a = FALSE, b = 0, c = "", d = NA))
})

test_that("na_to_none converts NA to NULL and passes other values through", {
  expect_null(na_to_none(NA))
  expect_null(na_to_none(NA_character_))
  expect_null(na_to_none(NA_integer_))
  expect_equal(na_to_none("x"), "x")
  expect_equal(na_to_none(0), 0)
  expect_null(na_to_none(NULL))
})

test_that("as_py_int coerces numeric-ish values to integer; preserves NULL", {
  expect_identical(as_py_int(5), 5L)
  expect_identical(as_py_int(5L), 5L)
  expect_identical(as_py_int(5.0), 5L)
  expect_null(as_py_int(NULL))
  expect_null(as_py_int(NA_integer_))
})

test_that("as_py_int rejects non-integral doubles", {
  expect_error(as_py_int(5.5), "integer")
})

test_that("as_py_list wraps atomic vectors as unnamed lists", {
  expect_equal(as_py_list(c("a", "b")), list("a", "b"))
  expect_equal(as_py_list(list("a", "b")), list("a", "b"))
  expect_equal(as_py_list("a"), list("a"))
  expect_null(as_py_list(NULL))
})

test_that("as_py_dict requires a named list", {
  expect_equal(as_py_dict(list(a = 1, b = 2)), list(a = 1, b = 2))
  expect_error(as_py_dict(list(1, 2)), "named")
  expect_null(as_py_dict(NULL))
})
