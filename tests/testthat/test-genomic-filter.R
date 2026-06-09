test_that("buildGenomicFilter forwards categorical values", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter("Gene_with_variant", values = c("BRCA1", "TP53"))
  expect_equal(gf$kind, "genomic_filter")
  expect_equal(gf$key, "Gene_with_variant")
  expect_equal(gf$values, c("BRCA1", "TP53"))
})

test_that("buildGenomicFilter coerces a single enum member to its value", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter(
    "Variant_frequency_as_text",
    values = picsure::VariantFrequency$RARE
  )
  expect_equal(gf$values, "Rare")
})

test_that("buildGenomicFilter coerces a list of mixed members and strings", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter(
    "chr5:148481541:T:A",
    values = list(picsure::Zygosity$HETEROZYGOUS, "1/1")
  )
  expect_equal(gf$values, c("0/1", "1/1"))
})

test_that("buildGenomicFilter forwards a numeric range", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter("Variant_frequency_in_gnomAD", min = 0, max = 0.01)
  expect_equal(gf$min, 0)
  expect_equal(gf$max, 0.01)
})

test_that("buildGenomicFilter rejects an empty key", {
  expect_error(picsure::buildGenomicFilter("", values = "x"), "non-empty")
})

test_that("buildQuery forwards genomicFilters", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter("Gene_with_variant", values = "BRCA1")
  q <- picsure::buildQuery(genomicFilters = gf)
  expect_equal(q$genomicFilters$key, "Gene_with_variant")
})
