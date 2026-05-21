# Tests for the picsure_enum_member S3 class and its core methods.
# Per-enum tests live in this file too (added in later tasks).

test_that(".enum_member builds a list with name, value, and class", {
  m <- picsure:::.enum_member(
    "FILTER", "filter",
    enum_name = "PhenotypicFilterType",
    subclass = "picsure_phenotypic_filter_type"
  )
  expect_equal(m$name, "FILTER")
  expect_equal(m$value, "filter")
  expect_s3_class(m, "picsure_phenotypic_filter_type")
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
    enum_name = "PhenotypicFilterType",
    subclass = "picsure_phenotypic_filter_type"
  )
  expect_equal(format(m), "<PhenotypicFilterType.FILTER>")
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
    enum_name = "PhenotypicFilterType",
    subclass = "picsure_phenotypic_filter_type"
  )
  expect_equal(as.character(m), "REQUIRE")
})

# PhenotypicFilterType

test_that("PhenotypicFilterType has 4 members with expected names and values", {
  expect_setequal(names(picsure::PhenotypicFilterType),
                  c("FILTER", "ANYRECORD", "SELECT", "REQUIRE"))
  expect_equal(picsure::PhenotypicFilterType$FILTER$name,    "FILTER")
  expect_equal(picsure::PhenotypicFilterType$FILTER$value,   "filter")
  expect_equal(picsure::PhenotypicFilterType$ANYRECORD$name, "ANYRECORD")
  expect_equal(picsure::PhenotypicFilterType$ANYRECORD$value, "anyrecord")
  expect_equal(picsure::PhenotypicFilterType$SELECT$name,    "SELECT")
  expect_equal(picsure::PhenotypicFilterType$SELECT$value,   "select")
  expect_equal(picsure::PhenotypicFilterType$REQUIRE$name,   "REQUIRE")
  expect_equal(picsure::PhenotypicFilterType$REQUIRE$value,  "require")
})

test_that("PhenotypicFilterType members are picsure_phenotypic_filter_type", {
  for (m in picsure::PhenotypicFilterType) {
    expect_s3_class(m, "picsure_phenotypic_filter_type")
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
  expect_equal(format(picsure::PhenotypicFilterType$FILTER),    "<PhenotypicFilterType.FILTER>")
  expect_equal(format(picsure::GroupOperator$AND),    "<GroupOperator.AND>")
  expect_equal(format(picsure::QueryType$COUNT),      "<QueryType.COUNT>")
})

# Platform

test_that("Platform has 8 members", {
  expect_setequal(names(picsure::Platform), c(
    "BDC_AUTHORIZED", "BDC_OPEN",
    "BDC_DEV_AUTHORIZED", "BDC_DEV_OPEN",
    "BDC_PREDEV_AUTHORIZED", "BDC_PREDEV_OPEN",
    "NHANES_AUTHORIZED", "NHANES_OPEN"
  ))
})

test_that("Platform$BDC_OPEN has expected fields", {
  m <- picsure::Platform$BDC_OPEN
  expect_equal(m$name, "BDC_OPEN")
  expect_equal(m$url, "https://picsure.biodatacatalyst.nhlbi.nih.gov")
  expect_equal(m$resource_uuid, "ac004461-1b47-4832-80e2-22a4aecabe39")
  expect_equal(m$label, "BDC Open")
  expect_false(m$include_consents)
  expect_false(m$requires_auth)
})

test_that("Platform$BDC_AUTHORIZED has expected fields", {
  m <- picsure::Platform$BDC_AUTHORIZED
  expect_equal(m$name, "BDC_AUTHORIZED")
  expect_equal(m$url, "https://picsure.biodatacatalyst.nhlbi.nih.gov")
  expect_equal(m$resource_uuid, "02e23f52-f354-4e8b-992c-d37c8b9ba140")
  expect_equal(m$label, "BDC Authorized")
  expect_true(m$include_consents)
  expect_true(m$requires_auth)
})

test_that("Platform$NHANES_AUTHORIZED has expected fields", {
  m <- picsure::Platform$NHANES_AUTHORIZED
  expect_equal(m$name, "NHANES_AUTHORIZED")
  expect_equal(m$url, "https://nhanes.hms.harvard.edu/")
  expect_equal(m$resource_uuid, "ded89b08-faa9-435c-b7c4-55b81922ee5f")
  expect_equal(m$label, "Nhanes Authorized")
  expect_false(m$include_consents)
  expect_true(m$requires_auth)
})

test_that("Platform members expose value as a flat-fields list", {
  m <- picsure::Platform$BDC_OPEN
  expect_equal(m$value$url, m$url)
  expect_equal(m$value$resource_uuid, m$resource_uuid)
  expect_equal(m$value$label, m$label)
  expect_equal(m$value$include_consents, m$include_consents)
  expect_equal(m$value$requires_auth, m$requires_auth)
})

test_that("Platform members are picsure_platform", {
  for (m in picsure::Platform) {
    expect_s3_class(m, "picsure_platform")
    expect_s3_class(m, "picsure_enum_member")
  }
})

test_that("print.picsure_platform shows attached fields", {
  out <- capture.output(print(picsure::Platform$BDC_OPEN))
  expect_match(out[1], "<Platform.BDC_OPEN>", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "url:", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "resource_uuid:", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "label:", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "include_consents:", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "requires_auth:", fixed = TRUE)
  joined <- paste(out, collapse = "\n")
  expect_match(joined, "https://picsure.biodatacatalyst.nhlbi.nih.gov", fixed = TRUE)
  expect_match(joined, "ac004461-1b47-4832-80e2-22a4aecabe39", fixed = TRUE)
  expect_match(joined, "BDC Open", fixed = TRUE)
})
