test_that("search() against a live platform returns a data.frame", {
  skip_unless_integration()
  session <- live_session()

  results <- picsure::search(session, "sex")
  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) > 0)
})

test_that("search() respects limit", {
  skip_unless_integration()
  session <- live_session()

  small <- picsure::search(session, "age", limit = 5)
  expect_s3_class(small, "data.frame")
  expect_true(nrow(small) <= 5)
})

test_that("facets() returns a FacetSet that addFacet can mutate", {
  skip_unless_integration()
  session <- live_session()

  fs <- picsure::facets(session)
  fs <- picsure::addFacet(fs, "study_ids", "phs000007")
  expect_true(inherits(fs, "python.builtin.object"))
})
