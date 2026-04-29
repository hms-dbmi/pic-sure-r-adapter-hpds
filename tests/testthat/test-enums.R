# Tests for the picsure_enum_member S3 class and its core methods.
# Per-enum tests live in this file too (added in later tasks).

test_that(".enum_member builds a list with name, value, and class", {
  m <- picsure:::.enum_member(
    "FILTER", "filter",
    enum_name = "ClauseType",
    subclass = "picsure_clause_type"
  )
  expect_equal(m$name, "FILTER")
  expect_equal(m$value, "filter")
  expect_s3_class(m, "picsure_clause_type")
  expect_s3_class(m, "picsure_enum_member")
})

test_that(".enum_member stores enum_name as an attribute", {
  m <- picsure:::.enum_member(
    "AND", "AND",
    enum_name = "GroupOperator",
    subclass = "picsure_group_operator"
  )
  expect_equal(attr(m, "enum_name"), "GroupOperator")
})

test_that(".enum_member accepts extra fields via ...", {
  m <- picsure:::.enum_member(
    "BDC_OPEN",
    list(url = "https://example", label = "BDC Open"),
    enum_name = "Platform",
    subclass = "picsure_platform",
    url = "https://example",
    label = "BDC Open"
  )
  expect_equal(m$url, "https://example")
  expect_equal(m$label, "BDC Open")
})

test_that("format.picsure_enum_member uses the enum_name attribute", {
  m <- picsure:::.enum_member(
    "FILTER", "filter",
    enum_name = "ClauseType",
    subclass = "picsure_clause_type"
  )
  expect_equal(format(m), "<ClauseType.FILTER>")
})

test_that("format.picsure_enum_member falls back to 'Enum' if attribute missing", {
  m <- structure(
    list(name = "X", value = "x"),
    class = c("picsure_enum_member")
  )
  expect_equal(format(m), "<Enum.X>")
})

test_that("print.picsure_enum_member writes format() output", {
  m <- picsure:::.enum_member(
    "OR", "OR",
    enum_name = "GroupOperator",
    subclass = "picsure_group_operator"
  )
  expect_output(print(m), "<GroupOperator.OR>", fixed = TRUE)
})

test_that("as.character.picsure_enum_member returns the name", {
  m <- picsure:::.enum_member(
    "REQUIRE", "require",
    enum_name = "ClauseType",
    subclass = "picsure_clause_type"
  )
  expect_equal(as.character(m), "REQUIRE")
})
