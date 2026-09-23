test_that("package exposes a non-NULL picsure_py binding after .onLoad", {
  py <- get("picsure_py", envir = asNamespace("picsure"), inherits = FALSE)
  # .onLoad assigns a reticulate module proxy (lazy); the exact class
  # depends on whether reticulate has initialized, so assert only that
  # the binding exists and is not NULL.
  expect_true(exists("picsure_py", envir = asNamespace("picsure"), inherits = FALSE))
  expect_false(is.null(py))
})

test_that("Python dependency pin parses as a direct reference to the upstream adapter commit", {
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
  expect_identical(sha, "70d567988e5b07108353e80a70cb886a5d25c3ea")
})

test_that(".picsure_pinned_sha reads the commit off the dependency spec", {
  expect_identical(
    picsure:::.picsure_pinned_sha(),
    "70d567988e5b07108353e80a70cb886a5d25c3ea"
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
  built <- picsure:::.picsure_build_sha("2.0.1.dev159+g70d567988")

  expect_true(startsWith(pinned, built))
  expect_false(startsWith(pinned, picsure:::.picsure_build_sha("2.0.1.dev11+ga023f367")))
})

test_that(".PICSURE_PY_TAG names the release tag at the pinned commit", {
  expect_identical(picsure:::.PICSURE_PY_TAG, "v3.0.0")
  expect_identical(
    picsure:::.picsure_pin_verdict("3.0.0", picsure:::.picsure_pinned_sha(), picsure:::.PICSURE_PY_TAG),
    "match"
  )
})

test_that(".picsure_pinned_tag prefers a recorded tag over the spec", {
  sha_spec <- "picsure @ git+https://example.test/x.git@0123456789abcdef0123456789abcdef01234567"
  expect_identical(picsure:::.picsure_pinned_tag(sha_spec, "v2.1.0"), "v2.1.0")
  expect_true(is.na(picsure:::.picsure_pinned_tag(sha_spec, NA_character_)))
})

test_that(".picsure_pinned_tag reads a version-shaped ref off the spec", {
  expect_identical(
    picsure:::.picsure_pinned_tag("picsure @ git+https://example.test/x.git@v2.1.0", NA_character_),
    "v2.1.0"
  )
  expect_identical(
    picsure:::.picsure_pinned_tag("picsure @ git+https://example.test/x.git@2.0.0rc1", NA_character_),
    "2.0.0rc1"
  )
  expect_true(is.na(picsure:::.picsure_pinned_tag("picsure @ git+https://example.test/x.git@main", NA_character_)))
  expect_true(is.na(picsure:::.picsure_pinned_tag("picsure==2.0.0", NA_character_)))
})

test_that(".picsure_pin_verdict classifies commit builds against the pinned commit", {
  pinned <- "0123456789abcdef0123456789abcdef01234567"
  expect_identical(picsure:::.picsure_pin_verdict("2.0.1.dev11+g0123456", pinned, NA_character_), "match")
  expect_identical(picsure:::.picsure_pin_verdict("2.0.1.dev11+g0123456.d20260918", pinned, NA_character_), "match")
  expect_identical(picsure:::.picsure_pin_verdict("2.0.1.dev11+gabcdef0", pinned, NA_character_), "mismatch")
  expect_identical(picsure:::.picsure_pin_verdict("2.0.1.dev11+gabcdef0", pinned, "v2.1.0"), "mismatch")
})

test_that(".picsure_pin_verdict accepts the bare tag version for a tagged pin", {
  pinned <- "0123456789abcdef0123456789abcdef01234567"
  expect_identical(picsure:::.picsure_pin_verdict("2.1.0", pinned, "v2.1.0"), "match")
  expect_identical(picsure:::.picsure_pin_verdict("2.1.0+d20260918", pinned, "v2.1.0"), "match")
  expect_identical(picsure:::.picsure_pin_verdict("2.1.0", NA_character_, "v2.1.0"), "match")
  expect_identical(picsure:::.picsure_pin_verdict("2.0.0", pinned, "v2.1.0"), "mismatch")
})

test_that(".picsure_pin_verdict is unconfirmed without a commit suffix or a tag to compare", {
  pinned <- "0123456789abcdef0123456789abcdef01234567"
  expect_identical(picsure:::.picsure_pin_verdict("2.0.0", pinned, NA_character_), "unconfirmed")
  expect_identical(picsure:::.picsure_pin_verdict(NA_character_, pinned, "v2.1.0"), "unconfirmed")
})

pin_check_state <- function() new.env(parent = emptyenv())

pin_check <- function(installed, spec = "picsure @ git+https://example.test/x.git@0123456789abcdef0123456789abcdef01234567",
                      tag = NA_character_, state = pin_check_state()) {
  withVisible(picsure:::.picsure_warn_on_pin_mismatch(
    spec = spec,
    tag = tag,
    installed_version = function() installed,
    module_path = function() "/venv/lib/python3.12/site-packages/picsure/__init__.py",
    state = state
  ))
}

test_that("pin check passes silently for a build carrying the pinned commit", {
  result <- NULL
  expect_no_condition(result <- pin_check("2.0.1.dev11+g0123456"))
  expect_true(result$value)
  expect_false(result$visible)
})

test_that("pin check warns on a build from a different commit", {
  result <- NULL
  expect_warning(
    result <- pin_check("2.0.1.dev11+gabcdef0"),
    "NOT the pinned one"
  )
  expect_false(result$value)
  expect_false(result$visible)
})

test_that("pin check warning names what was expected, what loaded, and where from", {
  warned <- tryCatch(pin_check("2.0.1.dev11+gabcdef0"), warning = identity)
  expect_match(conditionMessage(warned), "expected:        0123456789abcdef0123456789abcdef01234567", fixed = TRUE)
  expect_match(conditionMessage(warned), "actual version:  2.0.1.dev11+gabcdef0", fixed = TRUE)
  expect_match(conditionMessage(warned), "loaded from:     /venv/lib/python3.12/site-packages/picsure/__init__.py", fixed = TRUE)
  expect_match(conditionMessage(warned), "RETICULATE_PYTHON", fixed = TRUE)
  expect_match(conditionMessage(warned), "vignette('getting-started')", fixed = TRUE)
})

test_that("pin check accepts the tag form of a tagged pinned commit", {
  result <- NULL
  expect_no_condition(result <- pin_check("2.1.0", tag = "v2.1.0"))
  expect_true(result$value)

  expect_no_condition(result <- pin_check("2.1.0", spec = "picsure @ git+https://example.test/x.git@v2.1.0"))
  expect_true(result$value)
})

test_that("pin check warns when a tagged pin meets a different release or a commit build", {
  expect_warning(pin_check("2.0.0", tag = "v2.1.0"), "NOT the pinned one")
  expect_warning(
    pin_check("2.1.1.dev3+gabcdef0", spec = "picsure @ git+https://example.test/x.git@v2.1.0"),
    "NOT the pinned one"
  )
  warned <- tryCatch(pin_check("2.0.0", tag = "v2.1.0"), warning = identity)
  expect_match(
    conditionMessage(warned),
    "expected:        v2.1.0 (0123456789abcdef0123456789abcdef01234567)",
    fixed = TRUE
  )
})

test_that("pin check emits a message, not a warning, when the build cannot be confirmed", {
  result <- NULL
  expect_message(result <- pin_check("2.0.0"), "could not confirm")
  expect_false(result$value)

  expect_message(result <- pin_check(NA_character_), "no distribution metadata found")
  expect_false(result$value)

  failing <- NULL
  expect_message(
    failing <- withVisible(picsure:::.picsure_warn_on_pin_mismatch(
      spec = "picsure @ git+https://example.test/x.git@0123456789abcdef0123456789abcdef01234567",
      tag = NA_character_,
      installed_version = function() stop("no interpreter"),
      module_path = function() stop("no interpreter"),
      state = pin_check_state()
    )),
    "could not confirm"
  )
  expect_false(failing$value)
})

test_that("pin check stays quiet when the spec pins nothing comparable", {
  result <- NULL
  expect_no_condition(result <- pin_check("2.0.0", spec = "picsure==2.0.0"))
  expect_false(result$value)
})

test_that("pin check runs once per state and then replays its verdict", {
  state <- pin_check_state()
  expect_warning(pin_check("2.0.1.dev11+gabcdef0", state = state))
  expect_no_condition(replay <- pin_check("2.0.1.dev11+g0123456", state = state))
  expect_false(replay$value)

  matched <- pin_check_state()
  expect_no_condition(pin_check("2.0.1.dev11+g0123456", state = matched))
  expect_true(pin_check("2.0.1.dev11+gabcdef0", state = matched)$value)
})
