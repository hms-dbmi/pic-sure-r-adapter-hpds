test_that("platforms() returns the character vector of Platform enum member names", {
  fake <- fake_picsure_py(platform_names = c("Demo", "BDC Open", "BDC Authorized"))
  testthat::local_mocked_bindings(picsure_py = fake)

  result <- picsure::platforms()

  expect_type(result, "character")
  expect_true(length(result) >= 1L)
  expect_true("Demo" %in% result)
})

test_that("platforms() surfaces Python errors as picsureError", {
  fake <- fake_picsure_py()
  fake$Platform <- NULL
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(picsure::platforms(), error = function(e) e)
  expect_s3_class(err, "error")
})
