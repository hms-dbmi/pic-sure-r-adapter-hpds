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
  expect_equal(m$value, list(url = "https://example", label = "BDC Open"))
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

# ClauseType

test_that("ClauseType has 4 members with expected names and values", {
  expect_setequal(names(picsure::ClauseType),
                  c("FILTER", "ANYRECORD", "SELECT", "REQUIRE"))
  expect_equal(picsure::ClauseType$FILTER$name,    "FILTER")
  expect_equal(picsure::ClauseType$FILTER$value,   "filter")
  expect_equal(picsure::ClauseType$ANYRECORD$name, "ANYRECORD")
  expect_equal(picsure::ClauseType$ANYRECORD$value, "anyrecord")
  expect_equal(picsure::ClauseType$SELECT$name,    "SELECT")
  expect_equal(picsure::ClauseType$SELECT$value,   "select")
  expect_equal(picsure::ClauseType$REQUIRE$name,   "REQUIRE")
  expect_equal(picsure::ClauseType$REQUIRE$value,  "require")
})

test_that("ClauseType members are picsure_clause_type", {
  for (m in picsure::ClauseType) {
    expect_s3_class(m, "picsure_clause_type")
    expect_s3_class(m, "picsure_enum_member")
  }
})

# GroupOperator

test_that("GroupOperator has 2 members with name == value", {
  expect_setequal(names(picsure::GroupOperator), c("AND", "OR"))
  expect_equal(picsure::GroupOperator$AND$name,  "AND")
  expect_equal(picsure::GroupOperator$AND$value, "AND")
  expect_equal(picsure::GroupOperator$OR$name,   "OR")
  expect_equal(picsure::GroupOperator$OR$value,  "OR")
})

test_that("GroupOperator members are picsure_group_operator", {
  for (m in picsure::GroupOperator) {
    expect_s3_class(m, "picsure_group_operator")
    expect_s3_class(m, "picsure_enum_member")
  }
})

# QueryType

test_that("QueryType has 4 members with expected names and lowercase values", {
  expect_setequal(names(picsure::QueryType),
                  c("COUNT", "PARTICIPANT", "TIMESTAMP", "CROSS_COUNT"))
  expect_equal(picsure::QueryType$COUNT$name,         "COUNT")
  expect_equal(picsure::QueryType$COUNT$value,        "count")
  expect_equal(picsure::QueryType$PARTICIPANT$name,   "PARTICIPANT")
  expect_equal(picsure::QueryType$PARTICIPANT$value,  "participant")
  expect_equal(picsure::QueryType$TIMESTAMP$name,     "TIMESTAMP")
  expect_equal(picsure::QueryType$TIMESTAMP$value,    "timestamp")
  expect_equal(picsure::QueryType$CROSS_COUNT$name,   "CROSS_COUNT")
  expect_equal(picsure::QueryType$CROSS_COUNT$value,  "cross_count")
})

test_that("QueryType members are picsure_query_type", {
  for (m in picsure::QueryType) {
    expect_s3_class(m, "picsure_query_type")
    expect_s3_class(m, "picsure_enum_member")
  }
})

test_that("one member per simple enum formats as <EnumName.MEMBER>", {
  expect_equal(format(picsure::ClauseType$FILTER),    "<ClauseType.FILTER>")
  expect_equal(format(picsure::GroupOperator$AND),    "<GroupOperator.AND>")
  expect_equal(format(picsure::QueryType$COUNT),      "<QueryType.COUNT>")
})
