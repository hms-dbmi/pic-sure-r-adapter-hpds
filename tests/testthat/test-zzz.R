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
  expect_identical(sha, "0eec30d062751396e006284e79c19110408e4a62")
})

test_that(".picsure_pinned_sha reads the commit off the dependency spec", {
  expect_identical(
    picsure:::.picsure_pinned_sha(),
    "0eec30d062751396e006284e79c19110408e4a62"
  )
  expect_identical(
    picsure:::.picsure_pinned_sha("picsure @ git+https://example.test/x.git@0123456789abcdef0123456789abcdef01234567"),
    "0123456789abcdef0123456789abcdef01234567"
  )
})

test_that(".picsure_pinned_sha returns NA for a spec that pins no commit", {
  expect_true(is.na(picsure:::.picsure_pinned_sha("picsure==2.0.0")))
  expect_true(is.na(picsure:::.picsure_pinned_sha("picsure @ git+https://example.test/x.git@main")))
})

test_that(".picsure_build_sha reads the commit out of a hatch-vcs version", {
  expect_identical(picsure:::.picsure_build_sha("2.0.1.dev11+g0eec30d06"), "0eec30d06")
  expect_identical(
    picsure:::.picsure_build_sha("2.0.1.dev11+g0eec30d06.d20260910"),
    "0eec30d06"
  )
})

test_that(".picsure_build_sha returns NA for a version carrying no commit", {
  expect_true(is.na(picsure:::.picsure_build_sha("2.0.0")))
  expect_true(is.na(picsure:::.picsure_build_sha(NA_character_)))
  expect_true(is.na(picsure:::.picsure_build_sha(character(0))))
})

test_that("a hatch-vcs version built from the pinned commit is recognized as a match", {
  pinned <- picsure:::.picsure_pinned_sha()
  built <- picsure:::.picsure_build_sha("2.0.1.dev11+g0eec30d06")

  expect_true(startsWith(pinned, built))
  expect_false(startsWith(pinned, picsure:::.picsure_build_sha("2.0.1.dev11+ga023f367")))
})
