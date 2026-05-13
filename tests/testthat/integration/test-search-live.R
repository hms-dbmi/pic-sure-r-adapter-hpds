test_that("dictionarySearch() against a live platform returns a data.frame", {
  skip_unless_integration()
  session <- live_session()

  term <- Sys.getenv("PICSURE_TEST_SEARCH_TERM", unset = "age")
  results <- picsure::dictionarySearch(session, term)
  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) > 0)
})

test_that("dictionarySearch() with include_values = FALSE still returns rows", {
  skip_unless_integration()
  session <- live_session()

  term <- Sys.getenv("PICSURE_TEST_SEARCH_TERM", unset = "age")
  lean <- picsure::dictionarySearch(session, term, include_values = FALSE)
  expect_s3_class(lean, "data.frame")
  expect_true(nrow(lean) > 0)
})

test_that("facets() returns a FacetSet that addFacet can mutate", {
  skip_unless_integration()
  session <- live_session()

  fs <- picsure::facets(session)
  # data_source is a valid facet category on BDC deployments; override via
  # env var for other backends.
  category <- Sys.getenv("PICSURE_TEST_FACET_CATEGORY", unset = "data_source")
  value    <- Sys.getenv("PICSURE_TEST_FACET_VALUE", unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    testthat::skip("PICSURE_TEST_FACET_VALUE not set.")
  }

  fs <- picsure::addFacet(fs, category, value)
  expect_true(inherits(fs, "python.builtin.object"))
})
