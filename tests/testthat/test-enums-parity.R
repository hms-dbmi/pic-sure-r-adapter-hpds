# Drift test — verifies R-side enum definitions match the Python adapter.
# Skips when picsure_py is the testthat fake or hasn't been initialized.

skip_if_no_picsure_py <- function() {
  if (is.null(picsure:::picsure_py) ||
      !inherits(picsure:::picsure_py, "python.builtin.module")) {
    testthat::skip("picsure_py not initialized as a real Python module")
  }
}

py_members <- function(py_enum) {
  # __members__ returns a mappingproxy; dict() converts it to an R list keyed
  # by member name with each element being the enum member Python object.
  builtins <- reticulate::import_builtins()
  members <- builtins$dict(py_enum$`__members__`)
  lapply(members, function(m) list(name = m$name, value = m$value))
}

test_that("ClauseType matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$ClauseType)
  expect_setequal(names(picsure::ClauseType), names(py))
  for (n in names(picsure::ClauseType)) {
    expect_equal(picsure::ClauseType[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::ClauseType[[n]]$value, py[[n]]$value, info = n)
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
    expect_setequal(
      py_field_names,
      c("url", "resource_uuid", "label", "include_consents", "requires_auth")
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
  r_enums <- c("ClauseType", "GroupOperator", "Platform", "QueryType")
  expect_setequal(py_enums, r_enums)
})
