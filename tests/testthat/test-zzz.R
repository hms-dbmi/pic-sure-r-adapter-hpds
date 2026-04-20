test_that("package loads and exposes picsure_py binding", {
  expect_true(exists("picsure_py", envir = asNamespace("picsure"), inherits = FALSE))
  # After Task 12, .onLoad assigns a reticulate lazy-load module proxy;
  # picsure_py is no longer NULL.
  py <- get("picsure_py", envir = asNamespace("picsure"), inherits = FALSE)
  expect_true(inherits(py, "python.builtin.module"))
})
