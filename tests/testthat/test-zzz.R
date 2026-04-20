test_that("package loads and exposes picsure_py binding", {
  expect_true(exists("picsure_py", envir = asNamespace("picsure"), inherits = FALSE))
  expect_null(get("picsure_py", envir = asNamespace("picsure"), inherits = FALSE))
})
