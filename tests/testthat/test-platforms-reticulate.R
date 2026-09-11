# platforms() tests that cross the real reticulate boundary.
#
# platforms() shipped broken for every real caller while its tests were
# green. The fake stood `picsure_py$Platform` up as an R character vector, so
# the call took the non-Python branch and never reached the conversion. A real
# `Platform` is an Enum class, and `Platform.__members__` is a `mappingproxy`
# — a type reticulate has no converter for. `py_to_r()` returns the proxy
# unchanged, and the `vapply()` over it fails with "cannot coerce type
# 'environment' to vector of type 'list'".
#
# The synthetic enum below needs only a bare interpreter, so the conversion
# boundary is covered even where the picsure package is not installed.

py_platform_enum <- function() {
  reticulate::py_run_string(paste(
    "import enum",
    "from dataclasses import dataclass",
    "",
    "@dataclass(frozen=True)",
    "class _RAdapterPlatformConfig:",
    "    url: str",
    "    label: str",
    "",
    "class _RAdapterPlatform(enum.Enum):",
    "    DEMO = _RAdapterPlatformConfig('https://demo.example', 'Demo')",
    "    BDC_OPEN = _RAdapterPlatformConfig('https://open.example', 'BDC Open')",
    "    BDC_AUTHORIZED = _RAdapterPlatformConfig('https://auth.example', 'BDC Authorized')",
    sep = "\n"
  ))
  reticulate::py$`_RAdapterPlatform`
}

test_that("a Python enum's __members__ reaches R as an unconverted mappingproxy", {
  skip_unless_python_interpreter()
  mapping <- py_platform_enum()$`__members__`

  expect_s3_class(mapping, "python.builtin.object")
  expect_s3_class(reticulate::py_to_r(mapping), "python.builtin.object")
  expect_error(
    vapply(mapping, function(m) m$value$label, character(1)),
    "cannot coerce type 'environment'"
  )
})

test_that(".platform_members converts a mappingproxy to a plain named R list", {
  skip_unless_python_interpreter()
  members <- picsure:::.platform_members(py_platform_enum())

  expect_true(is.list(members))
  expect_false(inherits(members, "python.builtin.object"))
  expect_named(members, c("DEMO", "BDC_OPEN", "BDC_AUTHORIZED"))
})

test_that("platforms() reads labels off a real Python Platform enum", {
  skip_unless_python_interpreter()
  testthat::local_mocked_bindings(picsure_py = list(Platform = py_platform_enum()))

  result <- picsure::platforms()

  expect_type(result, "character")
  expect_equal(result, c("Demo", "BDC Open", "BDC Authorized"))
})

test_that("platforms() lists the labels the pinned Python adapter defines", {
  skip_unless_python_module("picsure")

  result <- picsure::platforms()

  expect_type(result, "character")
  expect_length(result, length(picsure::Platform))
  expect_setequal(
    result,
    vapply(picsure::Platform, function(m) m$label, character(1), USE.NAMES = FALSE)
  )
})
