test_that("facets() returns a FacetSet handle from the session", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  fs <- picsure::facets(bdc)

  expect_s3_class(fs, "fake_facet_set")
})

test_that("facets() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$facets <- function() {
    stop(structure(
      list(message = "facets endpoint unavailable"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(picsure::facets(bdc), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "facets endpoint", fixed = TRUE)
})
