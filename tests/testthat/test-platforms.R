test_that("platforms() returns the character vector of Platform enum member names", {
  fake <- fake_picsure_py(platform_names = c("Demo", "BDC Open", "BDC Authorized"))
  testthat::local_mocked_bindings(picsure_py = fake)

  result <- picsure::platforms()

  expect_type(result, "character")
  expect_true(length(result) >= 1L)
  expect_true("Demo" %in% result)
})

test_that(".platform_labels reads value$label from PlatformConfig-shaped members", {
  # Members shaped like the reticulate-converted Python Platform enum:
  # `value` is the PlatformConfig dataclass (read via $label), not a string.
  # Reading $value directly here would return a list, breaking vapply —
  # that was the bug this helper guards against.
  py_members <- list(
    BDC_OPEN = list(
      name = "BDC_OPEN",
      value = list(label = "BDC Open",       url = "https://open.example")
    ),
    BDC_AUTHORIZED = list(
      name = "BDC_AUTHORIZED",
      value = list(label = "BDC Authorized", url = "https://auth.example")
    )
  )

  result <- picsure:::.platform_labels(py_members)

  expect_type(result, "character")
  expect_equal(result, c("BDC Open", "BDC Authorized"))
})

test_that("platforms() surfaces Python errors as picsureError", {
  fake <- fake_picsure_py()
  fake$Platform <- NULL
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(picsure::platforms(), error = function(e) e)
  expect_s3_class(err, "error")
})
