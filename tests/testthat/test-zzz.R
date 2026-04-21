test_that("package exposes a non-NULL picsure_py binding after .onLoad", {
  py <- get("picsure_py", envir = asNamespace("picsure"), inherits = FALSE)
  # .onLoad assigns a reticulate module proxy (lazy); the exact class
  # depends on whether reticulate has initialized, so assert only that
  # the binding exists and is not NULL.
  expect_true(exists("picsure_py", envir = asNamespace("picsure"), inherits = FALSE))
  expect_false(is.null(py))
})
