# Drift test — verifies R-side enum definitions match the Python adapter.
# Skips when picsure_py is the testthat fake or hasn't been initialized.

skip_if_no_picsure_py <- function() {
  if (is.null(picsure:::picsure_py) ||
      !inherits(picsure:::picsure_py, "python.builtin.module")) {
    testthat::skip("picsure_py not initialized as a real Python module")
  }
}

py_members <- function(py_enum) {
  # __members__ maps member name -> member object. Reticulate coerces the
  # members of a string-based enum (class Foo(str, Enum)) to atomic R
  # characters on conversion, which breaks `m$name` / `m$value`. Compute the
  # name -> value mapping inside Python so only plain strings cross the
  # boundary, then reshape to the list(name=, value=) shape the tests use.
  reticulate::py_run_string(
    "def _picsure_parity_members(e):\n    return {name: member.value for name, member in e.__members__.items()}"
  )
  extractor <- reticulate::py_eval("_picsure_parity_members", convert = FALSE)
  vals <- reticulate::py_to_r(extractor(py_enum))
  stats::setNames(
    lapply(names(vals), function(n) list(name = n, value = vals[[n]])),
    names(vals)
  )
}

test_that("PhenotypicFilterType matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$PhenotypicFilterType)
  expect_setequal(names(picsure::PhenotypicFilterType), names(py))
  for (n in names(picsure::PhenotypicFilterType)) {
    expect_equal(picsure::PhenotypicFilterType[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::PhenotypicFilterType[[n]]$value, py[[n]]$value, info = n)
  }
})

test_that("GroupOperator matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$GroupOperator)
  expect_setequal(names(picsure::GroupOperator), names(py))
  for (n in names(picsure::GroupOperator)) {
    expect_equal(picsure::GroupOperator[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::GroupOperator[[n]]$value, py[[n]]$value, info = n)
  }
})

test_that("QueryType matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$QueryType)
  expect_setequal(names(picsure::QueryType), names(py))
  for (n in names(picsure::QueryType)) {
    expect_equal(picsure::QueryType[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::QueryType[[n]]$value, py[[n]]$value, info = n)
  }
})

test_that("Platform matches Python", {
  skip_if_no_picsure_py()
  builtins <- reticulate::import_builtins()
  py_enum <- picsure:::picsure_py$Platform
  py_names <- names(builtins$dict(py_enum$`__members__`))
  expect_setequal(names(picsure::Platform), py_names)
  for (n in names(picsure::Platform)) {
    py_cfg <- py_enum[[n]]$value
    r_cfg  <- picsure::Platform[[n]]
    py_field_names <- names(builtins$dict(py_cfg$`__dataclass_fields__`))
    # supports_genomic is a Python-only PlatformConfig field: R forwards it
    # through connect()'s `supports_genomic` kwarg rather than carrying it on
    # Platform members, so it is checked for presence here but not by value.
    expect_setequal(
      py_field_names,
      c("url", "resource_uuid", "label", "include_consents", "requires_auth", "supports_genomic")
    )
    expect_equal(r_cfg$url,              py_cfg$url,              info = n)
    expect_equal(r_cfg$resource_uuid,    py_cfg$resource_uuid,    info = n)
    expect_equal(r_cfg$label,            py_cfg$label,            info = n)
    expect_equal(r_cfg$include_consents, py_cfg$include_consents, info = n)
    expect_equal(r_cfg$requires_auth,    py_cfg$requires_auth,    info = n)
  }
})

test_that("All Python picsure enums are mirrored in R", {
  skip_if_no_picsure_py()
  reticulate::py_run_string(
    "import enum, picsure
_picsure_enum_names = sorted([
    name for name in dir(picsure)
    if not name.startswith('_')
    and isinstance(getattr(picsure, name, None), type)
    and issubclass(getattr(picsure, name), enum.Enum)
    and getattr(picsure, name) is not enum.Enum
    and len(getattr(picsure, name).__members__) > 0
])"
  )
  py_enums <- reticulate::py$`_picsure_enum_names`
  r_enums <- c(
    "PhenotypicFilterType", "GroupOperator", "Platform", "QueryType",
    "VariantFrequency", "GenomicFilterKey", "VariantSeverity"
  )
  expect_setequal(py_enums, r_enums)
})

test_that("VariantFrequency matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$VariantFrequency)
  expect_setequal(names(picsure::VariantFrequency), names(py))
  for (n in names(picsure::VariantFrequency)) {
    expect_equal(picsure::VariantFrequency[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::VariantFrequency[[n]]$value, py[[n]]$value, info = n)
  }
})

test_that("GenomicFilterKey matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$GenomicFilterKey)
  expect_setequal(names(picsure::GenomicFilterKey), names(py))
  for (n in names(picsure::GenomicFilterKey)) {
    expect_equal(picsure::GenomicFilterKey[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::GenomicFilterKey[[n]]$value, py[[n]]$value, info = n)
  }
})

test_that("VariantSeverity matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$VariantSeverity)
  expect_setequal(names(picsure::VariantSeverity), names(py))
  for (n in names(picsure::VariantSeverity)) {
    expect_equal(picsure::VariantSeverity[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::VariantSeverity[[n]]$value, py[[n]]$value, info = n)
  }
})

