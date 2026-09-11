test_that("searchGenomicValues delegates to session and returns a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  df <- picsure::searchGenomicValues(bdc, "Gene_with_variant", query = "BRCA", size = 10)
  expect_s3_class(df, "data.frame")
  expect_equal(df$value, c("BRCA1", "BRCA2"))
  call <- bdc$.calls$searchGenomicValues[[1]]
  expect_equal(call$genomicConceptPath, "Gene_with_variant")
  expect_equal(call$query, "BRCA")
  expect_equal(call$size, 10L)
})

test_that("searchGenomicValues rejects an empty key", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  expect_error(picsure::searchGenomicValues(bdc, ""), "non-empty")
})

test_that("genomicConsequences returns the vocabulary data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  df <- picsure::genomicConsequences()
  expect_s3_class(df, "data.frame")
  expect_true(all(c("severity", "consequence") %in% names(df)))
})

test_that("connect accepts supports_genomic as a known kwarg", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  expect_silent(
    picsure::connect(platform = "https://picsure.test", token = "tok", supports_genomic = TRUE)
  )
})

# RR-10: page and size are validated instead of silently coerced

test_that("searchGenomicValues forwards validated integers for page and size", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  picsure::searchGenomicValues(bdc, "Gene_with_variant", page = 3, size = 25L)

  call <- bdc$.calls$searchGenomicValues[[1]]
  expect_identical(call$page, 3L)
  expect_identical(call$size, 25L)
})

test_that("searchGenomicValues rejects a fractional page instead of truncating it", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  err <- tryCatch(
    picsure::searchGenomicValues(bdc, "Gene_with_variant", page = 1.7),
    condition = function(e) e
  )

  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "`page`", fixed = TRUE)
  expect_match(conditionMessage(err), "1.7", fixed = TRUE)
  expect_length(bdc$.calls$searchGenomicValues, 0L)
})

test_that("searchGenomicValues rejects a non-numeric page instead of sending NA", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  # The old behaviour: as.integer("two") warned "NAs introduced by coercion"
  # and forwarded NA. Nothing may be sent, and nothing may warn.
  err <- tryCatch(
    picsure::searchGenomicValues(bdc, "Gene_with_variant", page = "two"),
    condition = function(e) e
  )

  expect_s3_class(err, "picsureValidationError")
  expect_false(inherits(err, "warning"))
  expect_match(conditionMessage(err), "the string \"two\"", fixed = TRUE)
  expect_length(bdc$.calls$searchGenomicValues, 0L)
})

test_that("searchGenomicValues rejects every unusable page and size", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  for (bad in list(0, -1, 2.5, NA_integer_, NaN, Inf, "3", TRUE, c(1, 2), NULL)) {
    page_err <- tryCatch(
      picsure::searchGenomicValues(bdc, "Gene_with_variant", page = bad),
      condition = function(e) e
    )
    expect_s3_class(page_err, "picsureValidationError")
    expect_match(conditionMessage(page_err), "`page`", fixed = TRUE)

    size_err <- tryCatch(
      picsure::searchGenomicValues(bdc, "Gene_with_variant", size = bad),
      condition = function(e) e
    )
    expect_s3_class(size_err, "picsureValidationError")
    expect_match(conditionMessage(size_err), "`size`", fixed = TRUE)
  }
  expect_length(bdc$.calls$searchGenomicValues, 0L)
})

test_that("searchGenomicValues names the argument, not just the type", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")

  err <- tryCatch(
    picsure::searchGenomicValues(bdc, "Gene_with_variant", size = -5),
    condition = function(e) e
  )
  expect_match(conditionMessage(err), "`size`", fixed = TRUE)
  expect_match(conditionMessage(err), "-5", fixed = TRUE)
})

test_that("searchGenomicValues types its result column", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  bdc$searchGenomicValues <- function(...) data.frame(value = character())

  df <- picsure::searchGenomicValues(bdc, "Gene_with_variant")

  expect_equal(nrow(df), 0L)
  expect_type(df$value, "character")
})
