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
