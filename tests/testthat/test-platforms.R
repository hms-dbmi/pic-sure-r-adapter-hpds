# Pure-R tests for the label extraction platforms() performs after the Python
# boundary. The boundary itself — where the bug lived — is crossed in
# test-platforms-reticulate.R; a fake that hands back an R character vector
# cannot reproduce a reticulate conversion failure.

test_that(".platform_labels reads value$label from PlatformConfig-shaped members", {
  # Members shaped like the converted Python Platform enum: `value` is the
  # PlatformConfig dataclass (read via $label), not a string. Reading $value
  # directly would return the dataclass and break vapply(..., character(1)).
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

test_that(".platform_labels passes a character vector of labels straight through", {
  labels <- c(DEMO = "Demo", BDC_OPEN = "BDC Open")

  result <- picsure:::.platform_labels(labels)

  expect_type(result, "character")
  expect_equal(result, c("Demo", "BDC Open"))
  expect_null(names(result))
})

test_that(".platform_members leaves non-Python input alone", {
  members <- list(DEMO = list(value = list(label = "Demo")))

  expect_identical(picsure:::.platform_members(members), members)
  expect_identical(picsure:::.platform_members(c("Demo")), "Demo")
})

test_that("platforms() surfaces a missing Platform binding as an error", {
  fake <- fake_picsure_py()
  fake$Platform <- NULL
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(picsure::platforms(), error = function(e) e)
  expect_s3_class(err, "error")
})
