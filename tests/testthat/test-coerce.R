test_that("drop_nulls removes NULL entries from a list", {
  expect_equal(drop_nulls(list(a = 1, b = NULL, c = "x")), list(a = 1, c = "x"))
  expect_equal(drop_nulls(list()), list())
  expect_equal(drop_nulls(list(a = NULL, b = NULL)), list())
})

test_that("drop_nulls preserves non-NULL falsy values", {
  expect_equal(drop_nulls(list(a = FALSE, b = 0, c = "", d = NA)),
               list(a = FALSE, b = 0, c = "", d = NA))
})

test_that("to_py_enum resolves a case-insensitive string against the enum's members", {
  fake_enum <- list(FILTER = "python_FILTER", ANYRECORD = "python_ANYRECORD")
  expect_equal(to_py_enum("FILTER", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"), "python_FILTER")
  expect_equal(to_py_enum("filter", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"), "python_FILTER")
  expect_equal(to_py_enum("Anyrecord", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"), "python_ANYRECORD")
})

test_that("to_py_enum errors on unknown string with a helpful message listing valid values", {
  fake_enum <- list(FILTER = "x", ANYRECORD = "y")
  err <- tryCatch(to_py_enum("BOGUS", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
                  error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
  expect_match(err$message, "FILTER", fixed = TRUE)
  expect_match(err$message, "ANYRECORD", fixed = TRUE)
})

test_that("to_py_enum passes NULL through unchanged", {
  expect_null(to_py_enum(NULL, list(FILTER = "x"), "PhenotypicFilterType", "picsure_phenotypic_filter_type"))
})

# as_enum_string

test_that("as_enum_string returns NULL when value is NULL", {
  expect_null(picsure:::as_enum_string(NULL, "picsure_phenotypic_filter_type", "PhenotypicFilterType"))
})

test_that("as_enum_string returns the string itself for character input", {
  expect_equal(
    picsure:::as_enum_string("FILTER", "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    "FILTER"
  )
})

test_that("as_enum_string extracts the $name field by default for member input", {
  m <- picsure::PhenotypicFilterType$FILTER
  expect_equal(
    picsure:::as_enum_string(m, "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    "FILTER"
  )
})

test_that("as_enum_string extracts $value when field='value'", {
  m <- picsure::PhenotypicFilterType$FILTER
  expect_equal(
    picsure:::as_enum_string(m, "picsure_phenotypic_filter_type", "PhenotypicFilterType", field = "value"),
    "filter"
  )
})

test_that("as_enum_string rejects a member of the wrong subclass", {
  m <- picsure::GroupOperator$AND
  err <- tryCatch(
    picsure:::as_enum_string(m, "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
  expect_match(err$message, "GroupOperator", fixed = TRUE)
})

test_that("as_enum_string rejects non-string non-member input", {
  err <- tryCatch(
    picsure:::as_enum_string(42, "picsure_phenotypic_filter_type", "PhenotypicFilterType"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
})

# to_py_enum extended

test_that("to_py_enum unwraps a member of the right subclass and resolves it via the proxy", {
  fake_enum <- list(FILTER = "python_FILTER", ANYRECORD = "python_ANYRECORD")
  m <- picsure::PhenotypicFilterType$FILTER
  expect_equal(
    to_py_enum(m, fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
    "python_FILTER"
  )
})

test_that("to_py_enum still accepts case-insensitive strings (backwards compat)", {
  fake_enum <- list(FILTER = "python_FILTER")
  expect_equal(
    to_py_enum("filter", fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
    "python_FILTER"
  )
})

test_that("to_py_enum rejects a member of the wrong subclass before doing proxy lookup", {
  fake_enum <- list(FILTER = "python_FILTER")
  m <- picsure::GroupOperator$AND
  err <- tryCatch(
    to_py_enum(m, fake_enum, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
  expect_match(err$message, "GroupOperator", fixed = TRUE)
})

test_that("as_positive_whole_number accepts a positive whole number as an integer", {
  expect_identical(picsure:::as_positive_whole_number(1, "page"), 1L)
  expect_identical(picsure:::as_positive_whole_number(100L, "size"), 100L)
  expect_identical(picsure:::as_positive_whole_number(2.0, "page"), 2L)
})

test_that("as_positive_whole_number rejects a fractional value instead of truncating", {
  err <- tryCatch(picsure:::as_positive_whole_number(1.7, "page"), error = function(e) e)
  expect_s3_class(err, "picsureValidationError")
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "`page`", fixed = TRUE)
  expect_match(conditionMessage(err), "whole number", fixed = TRUE)
  expect_match(conditionMessage(err), "1.7", fixed = TRUE)
})

test_that("as_positive_whole_number rejects a non-numeric value instead of making it NA", {
  err <- tryCatch(picsure:::as_positive_whole_number("two", "page"), error = function(e) e)
  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "`page`", fixed = TRUE)
  expect_match(conditionMessage(err), "the string \"two\"", fixed = TRUE)
})

test_that("as_positive_whole_number names the argument and the value in every rejection", {
  bad <- list(
    NULL, NA, NA_integer_, NaN, Inf, -Inf, 0, -3, 2.5, "5", TRUE,
    c(1, 2), character(0), list(1)
  )
  for (value in bad) {
    err <- tryCatch(picsure:::as_positive_whole_number(value, "size"), error = function(e) e)
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`size`", fixed = TRUE)
  }
})

test_that("as_positive_whole_number rejects a value too large for an R integer", {
  err <- tryCatch(picsure:::as_positive_whole_number(3e9, "size"), error = function(e) e)
  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "2147483647", fixed = TRUE)
})

test_that("check_optional_number allows NULL, negatives and fractions but not nonsense", {
  expect_null(picsure:::check_optional_number(NULL, "min"))
  expect_identical(picsure:::check_optional_number(40L, "min"), 40L)
  expect_identical(picsure:::check_optional_number(-2.5, "min"), -2.5)

  for (value in list(NA_real_, Inf, "low", c(1, 2), TRUE, list(1))) {
    err <- tryCatch(picsure:::check_optional_number(value, "min"), error = function(e) e)
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`min`", fixed = TRUE)
  }
})

test_that("as_single_string rejects everything that is not one non-empty string", {
  expect_identical(picsure:::as_single_string("x", "term"), "x")

  for (value in list(NULL, NA_character_, "", 42, c("a", "b"), character(0), list("a"))) {
    err <- tryCatch(picsure:::as_single_string(value, "term"), error = function(e) e)
    expect_s3_class(err, "picsureValidationError")
    expect_match(conditionMessage(err), "`term`", fixed = TRUE)
  }
})

test_that("describe_argument_value says what actually arrived", {
  expect_equal(picsure:::describe_argument_value(NULL), "NULL")
  expect_equal(picsure:::describe_argument_value(character(0)), "an empty character vector")
  expect_equal(picsure:::describe_argument_value("hi"), "the string \"hi\"")
  expect_equal(picsure:::describe_argument_value(NA_integer_), "integer NA")
  expect_match(picsure:::describe_argument_value(c(1, 2, 3)), "3 values", fixed = TRUE)
  expect_match(picsure:::describe_argument_value(1:9), "...", fixed = TRUE)
  expect_match(picsure:::describe_argument_value(1.5), "1.5", fixed = TRUE)
})

test_that("to_py_enum reports a case-insensitive tie instead of guessing", {
  tied <- list(Rare = "a", RARE = "b")
  err <- tryCatch(
    to_py_enum("rare", tied, "VariantFrequency", "picsure_variant_frequency"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureValidationError")
  expect_match(conditionMessage(err), "matches 2 members", fixed = TRUE)
})

test_that("to_py_enum prefers an exact member-name match over a case-insensitive one", {
  tied <- list(Rare = "mixed", RARE = "upper")
  expect_equal(
    to_py_enum("RARE", tied, "VariantFrequency", "picsure_variant_frequency"),
    "upper"
  )
})

test_that("to_py_enum's rejection is a picsureError naming the enum", {
  err <- tryCatch(
    to_py_enum("BOGUS", list(FILTER = "x"), "PhenotypicFilterType",
               "picsure_phenotypic_filter_type"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureValidationError")
  expect_s3_class(err, "picsureError")
})

test_that("apply_result_schema types an empty frame the same as a full one", {
  schema <- c(a = "character", b = "numeric", c = "logical", d = "integer", e = "list")
  empty <- data.frame(a = character(), b = character(), c = character(),
                      d = character(), e = character(), stringsAsFactors = FALSE)
  full <- data.frame(a = "x", b = 1.5, c = TRUE, d = 3L, stringsAsFactors = FALSE)
  full$e <- list(c("p", "q"))

  typed_empty <- picsure:::apply_result_schema(empty, schema)
  typed_full  <- picsure:::apply_result_schema(full, schema)

  types <- function(d) vapply(d, function(x) class(x)[[1L]], character(1))
  expect_identical(types(typed_empty), types(typed_full))
  expect_identical(unname(types(typed_empty)),
                   c("character", "numeric", "logical", "integer", "list"))
  expect_equal(nrow(typed_empty), 0L)
  expect_equal(nrow(typed_full), 1L)
})

test_that("apply_result_schema leaves columns the schema does not name alone", {
  schema <- c(known = "numeric")
  data <- data.frame(known = "1", `\\phs1\\bmi\\` = "27.4",
                     check.names = FALSE, stringsAsFactors = FALSE)

  out <- picsure:::apply_result_schema(data, schema)

  expect_type(out$known, "double")
  expect_type(out[["\\phs1\\bmi\\"]], "character")
  expect_identical(names(out), names(data))
})

test_that("apply_result_schema does not add, drop, or reorder columns", {
  schema <- c(b = "numeric", missing_column = "character", a = "character")
  data <- data.frame(a = "x", b = "2", stringsAsFactors = FALSE)

  out <- picsure:::apply_result_schema(data, schema)

  expect_identical(names(out), c("a", "b"))
})

test_that("apply_result_schema returns a non-data-frame unchanged", {
  expect_null(picsure:::apply_result_schema(NULL, c(a = "character")))
  expect_equal(picsure:::apply_result_schema(42, c(a = "character")), 42)
  expect_equal(picsure:::apply_result_schema("text", c(a = "character")), "text")
})

test_that("coerce_result_column collapses a list cell rather than deparsing it", {
  out <- picsure:::coerce_result_column(list(c("a", "b"), character(0), "c"), "character")
  expect_identical(out, c("a, b", NA_character_, "c"))
})

test_that("coerce_result_column makes a list column out of an atomic one", {
  expect_identical(picsure:::coerce_result_column(c("a", "b"), "list"), list("a", "b"))
  expect_identical(picsure:::coerce_result_column(character(0), "list"), list())
})

test_that("the dictionary schema matches the Python DictionaryEntry fields", {
  expect_identical(
    names(picsure:::.DICTIONARY_RESULT_SCHEMA),
    c("conceptPath", "name", "display", "description", "dataType", "studyId",
      "values", "min", "max", "allowFiltering", "meta", "studyAcronym")
  )
})

test_that("the timeseries schema matches the header HPDS writes", {
  expect_identical(
    names(picsure:::.TIMESERIES_RESULT_SCHEMA),
    c("PATIENT_NUM", "CONCEPT_PATH", "NVAL_NUM", "TVAL_CHAR", "TIMESTAMP")
  )
  expect_identical(
    unname(picsure:::.TIMESERIES_RESULT_SCHEMA),
    c("integer", "character", "numeric", "character", "character")
  )
})

test_that("coerce_result_column warns once per column, naming it and the values lost", {
  expect_warning(
    out <- picsure:::coerce_result_column(c("12.5", "not a number", "", "7"), "numeric", "min"),
    "Column `min` could not be fully coerced to numeric: 2 values became NA",
    fixed = TRUE
  )
  expect_identical(out, c(12.5, NA, NA, 7))

  expect_warning(
    picsure:::coerce_result_column(c("TRUE", "maybe"), "logical", "allowFiltering"),
    "Column `allowFiltering` could not be fully coerced to logical: 1 value became NA",
    fixed = TRUE
  )
})

test_that("coerce_result_column stays silent when nothing is lost", {
  expect_silent(picsure:::coerce_result_column(c("1", NA, "3"), "integer", "PATIENT_NUM"))
  expect_silent(picsure:::coerce_result_column(character(0), "numeric", "min"))
  expect_silent(picsure:::coerce_result_column(c("a", "b"), "character", "name"))
})

test_that("apply_result_schema returns the frame and warns with the column name", {
  data <- data.frame(min = c("1", "x"), max = c("2", "3"), stringsAsFactors = FALSE)

  expect_warning(
    out <- picsure:::apply_result_schema(data, c(min = "numeric", max = "numeric")),
    "Column `min`", fixed = TRUE
  )
  expect_identical(out$min, c(1, NA))
  expect_identical(out$max, c(2, 3))
})
