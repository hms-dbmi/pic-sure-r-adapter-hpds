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
    "Variant_frequency_as_text",
    values = list(picsure::VariantFrequency$RARE, "Common")
  )
  expect_equal(gf$values, c("Rare", "Common"))
})

# Numeric range filtering was removed to match the categorical-only genomic
# filters the PIC-SURE frontend sends. `min` / `max` are therefore not
# parameters of the wrapper: they fall into `...` and reach Python as unknown
# kwargs, whose signature rejects them. The earlier version of this test only
# read `gf$min` / `gf$max` off a filter built WITHOUT them, which the fake
# never returns either way, so it asserted nothing about min/max at all.
test_that("buildGenomicFilter does not accept numeric range (min/max) args", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")

  expect_false(any(c("min", "max") %in% names(formals(picsure::buildGenomicFilter))))

  plain <- picsure::buildGenomicFilter("Gene_with_variant", values = "BRCA1")
  expect_false("min" %in% names(plain$extra))
  expect_false("max" %in% names(plain$extra))

  ranged <- picsure::buildGenomicFilter(
    "Gene_with_variant", values = "BRCA1", min = 1, max = 2
  )
  expect_identical(ranged$extra$min, 1)
  expect_identical(ranged$extra$max, 2)
  expect_null(ranged$min)
  expect_null(ranged$max)
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

test_that("buildGenomicFilter coerces a GenomicFilterKey member to its wire string", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter(
    picsure::GenomicFilterKey$GENE_WITH_VARIANT,
    values = "BRCA1"
  )
  expect_equal(gf$key, "Gene_with_variant")
})

test_that("buildGenomicFilter forwards a plain string key unchanged", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter("Variant_class", values = "SNV")
  expect_equal(gf$key, "Variant_class")
})

test_that("buildGenomicFilter rejects a wrong-subclass enum member as key", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  expect_error(
    picsure::buildGenomicFilter(picsure::VariantFrequency$RARE, values = "x"),
    "GenomicFilterKey"
  )
})

test_that("buildGenomicFilter coerces VariantSeverity member values to labels", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  gf <- picsure::buildGenomicFilter(
    picsure::GenomicFilterKey$VARIANT_SEVERITY,
    values = picsure::VariantSeverity$HIGH
  )
  expect_equal(gf$key, "Variant_severity")
  expect_equal(gf$values, "High Severity")
})

test_that("buildGenomicFilter with no key gives the friendly message", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py(), .package = "picsure")
  expect_error(picsure::buildGenomicFilter(values = "x"), "non-empty character scalar")
})
