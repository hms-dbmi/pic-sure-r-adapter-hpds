# Pure-R tests for the label extraction platforms() performs after the Python
# boundary. The boundary itself, where the bug lived, is crossed in
# test-platforms-reticulate.R. A fake that hands back an R character vector
# cannot reproduce a reticulate conversion failure.

test_that(".platform_labels reads value$label from PlatformConfig-shaped members", {
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

test_that(".py_enum_members leaves non-Python input alone", {
  members <- list(DEMO = list(value = list(label = "Demo")))

  expect_identical(picsure:::.py_enum_members(members), members)
  expect_identical(picsure:::.py_enum_members(c("Demo")), "Demo")
})
