test_that("package exposes a non-NULL picsure_py binding after .onLoad", {
  py <- get("picsure_py", envir = asNamespace("picsure"), inherits = FALSE)
  # .onLoad assigns a reticulate module proxy (lazy); the exact class
  # depends on whether reticulate has initialized, so assert only that
  # the binding exists and is not NULL.
  expect_true(exists("picsure_py", envir = asNamespace("picsure"), inherits = FALSE))
  expect_false(is.null(py))
})

test_that("Python dependency pin parses as the consent-routing adapter direct reference", {
  spec <- get(".PICSURE_PY_SPEC", envir = asNamespace("picsure"), inherits = FALSE)
  parts <- strsplit(spec, " @ git+", fixed = TRUE)[[1L]]

  expect_identical(parts[[1L]], "picsure")
  expect_length(parts, 2L)

  reference <- parts[[2L]]
  sha_start <- regexpr("@[0-9a-f]{40}$", reference, perl = TRUE)

  expect_gt(sha_start[[1L]], 0L)
  repository <- substr(reference, 1L, sha_start[[1L]] - 1L)
  sha <- substr(reference, sha_start[[1L]] + 1L, nchar(reference))

  expect_identical(repository, "https://github.com/hms-dbmi/pic-sure-python-adapter-hpds.git")
  expect_identical(sha, "a023f3678254ede43c9fee966f20070d3372ee11")
})
