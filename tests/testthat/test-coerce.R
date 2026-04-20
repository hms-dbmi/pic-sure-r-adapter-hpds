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
