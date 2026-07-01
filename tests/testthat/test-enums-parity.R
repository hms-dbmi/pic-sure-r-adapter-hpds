# Drift test — verifies R-side enum definitions match the Python adapter.
# Skips when picsure_py is the testthat fake or hasn't been initialized.

skip_if_no_picsure_py <- function() {
  if (is.null(picsure:::picsure_py) ||
      !inherits(picsure:::picsure_py, "python.builtin.module")) {
    testthat::skip("picsure_py not initialized as a real Python module")
  }
}

# Transitional guard: the R adapter added genomic-filtering enums/members
# ahead of the pinned Python adapter (.PICSURE_PY_SPEC @ main). While the
# installed Python lacks them, skip rather than fail; strict checking
# auto-resumes once the Python genomic-filtering change lands on main.
# TODO: remove these guards after the Python change is merged.
py_has_attr <- function(name) {
  reticulate::py_has_attr(picsure:::picsure_py, name)
}

py_members <- function(py_enum) {
  # Read each member's .name/.value on the Python side and return a plain
  # dict of strings. Enums whose members subclass a scalar (e.g.
  # `class X(str, Enum)`) are auto-converted by reticulate to atomic R
  # vectors, so accessing `m$name` after conversion fails with
  # "$ operator is invalid for atomic vectors". Building the mapping in
  # Python avoids that: the members are real enum objects there.
  reticulate::py_run_string(
    "def _picsure_enum_members(enum_cls):\n    return {m.name: {'name': m.name, 'value': m.value} for m in enum_cls}\n"
  )
  reticulate::py$`_picsure_enum_members`(py_enum)
}

test_that("PhenotypicFilterType matches Python", {
  skip_if_no_picsure_py()
  py <- py_members(picsure:::picsure_py$PhenotypicFilterType)
  # Transitional guard: the R adapter dropped SELECT ahead of the pinned
  # Python adapter (.PICSURE_PY_SPEC @ main). While the installed Python
  # still exposes SELECT, skip rather than fail; this auto-resumes strict
  # checking once the Python SELECT removal lands on main.
  # TODO: remove this guard after the Python change is merged.
  if ("SELECT" %in% names(py)) {
    skip("Pinned Python adapter still exposes PhenotypicFilterType.SELECT; pending its removal on main.")
  }
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
  new_members <- c(
    "VARIANT_COUNT", "VARIANT_LIST", "VCF_EXCERPT", "AGGREGATE_VCF_EXCERPT"
  )
  if (!all(new_members %in% names(py))) {
    skip("Pinned Python adapter lacks the variant QueryType members; pending its update on main.")
  }
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
      c("url", "resource_uuid", "label", "include_consents", "requires_auth", "supports_genomic")
    )
    expect_equal(r_cfg$url,              py_cfg$url,              info = n)
    expect_equal(r_cfg$resource_uuid,    py_cfg$resource_uuid,    info = n)
    expect_equal(r_cfg$label,            py_cfg$label,            info = n)
    expect_equal(r_cfg$include_consents, py_cfg$include_consents, info = n)
    expect_equal(r_cfg$requires_auth,    py_cfg$requires_auth,    info = n)
    expect_equal(r_cfg$supports_genomic, py_cfg$supports_genomic, info = n)
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
    "VariantFrequency"
  )
  if (!("VariantFrequency" %in% py_enums)) {
    skip("Pinned Python adapter lacks VariantFrequency enum; pending its update on main.")
  }
  expect_setequal(py_enums, r_enums)
})

test_that("VariantFrequency matches Python", {
  skip_if_no_picsure_py()
  if (!py_has_attr("VariantFrequency")) {
    skip("Pinned Python adapter lacks VariantFrequency; pending its update on main.")
  }
  py <- py_members(picsure:::picsure_py$VariantFrequency)
  expect_setequal(names(picsure::VariantFrequency), names(py))
  for (n in names(picsure::VariantFrequency)) {
    expect_equal(picsure::VariantFrequency[[n]]$name,  py[[n]]$name,  info = n)
    expect_equal(picsure::VariantFrequency[[n]]$value, py[[n]]$value, info = n)
  }
})

