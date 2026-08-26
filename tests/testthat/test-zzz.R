test_that("package exposes a non-NULL picsure_py binding after .onLoad", {
  py <- get("picsure_py", envir = asNamespace("picsure"), inherits = FALSE)
  # .onLoad assigns a reticulate module proxy (lazy); the exact class
  # depends on whether reticulate has initialized, so assert only that
  # the binding exists and is not NULL.
  expect_true(exists("picsure_py", envir = asNamespace("picsure"), inherits = FALSE))
  expect_false(is.null(py))
})

test_that("Python dependency is pinned to the consent-routing adapter", {
  spec <- get(".PICSURE_PY_SPEC", envir = asNamespace("picsure"), inherits = FALSE)

  expect_match(spec, "@e52a78e17c4fe2e575f0004483c647184b761115$", fixed = FALSE)
})
