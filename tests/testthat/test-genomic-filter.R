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
    "chr5,148481541,T,A",
    values = list(picsure::Zygosity$HETEROZYGOUS, "1/1")
  )
  expect_equal(gf$values, c("0/1", "1/1"))
})

test_that("buildGenomicFilter does not accept numeric range (min/max) args", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  # Numeric range filtering was removed to match the categorical-only genomic
  # filters the PIC-SURE frontend sends; min/max are forwarded as unknown
  # kwargs, which the Python adapter rejects.
  gf <- picsure::buildGenomicFilter("Gene_with_variant", values = "BRCA1")
  expect_null(gf$min)
  expect_null(gf$max)
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
