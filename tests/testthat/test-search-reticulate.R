# Dictionary-result typing, across the real reticulate boundary.
#
# A fake cannot show this defect. An R data.frame built in a test already has
# the types the test chose, and the problem is what pandas hands back. The
# Python adapter builds a matched-nothing result as
# `pd.DataFrame(columns=[...])`, whose every column is dtype `object`, and
# reticulate converts `object` to character. So `min`, `max`, and
# `allowFiltering` arrived as character on an empty search and as numeric,
# numeric, and logical on any other, and downstream arithmetic broke only
# when the search found nothing.

# Builds the two frames the Python adapter's two branches produce, with the
# same columns and dtypes as `_entries_to_dataframe()` and the empty-result
# branch of `search_dictionary()`.
dictionary_frames <- function() {
  reticulate::py_run_string("
import pandas as pd

_DICT_COLUMNS = [
    'conceptPath', 'name', 'display', 'description', 'dataType', 'studyId',
    'values', 'min', 'max', 'allowFiltering', 'meta', 'studyAcronym',
]

_dict_empty = pd.DataFrame(columns=_DICT_COLUMNS)

_dict_full = pd.DataFrame({
    'conceptPath': ['\\\\phs1\\\\bmi\\\\'],
    'name': ['bmi'],
    'display': ['BMI'],
    'description': ['Body mass index'],
    'dataType': ['Continuous'],
    'studyId': ['phs000001'],
    'values': [[]],
    'min': [12.5],
    'max': [61.0],
    'allowFiltering': [True],
    'meta': [None],
    'studyAcronym': ['FHS'],
})
")
  list(empty = reticulate::py$`_dict_empty`, full = reticulate::py$`_dict_full`)
}

column_types <- function(data) {
  vapply(data, function(column) class(column)[[1L]], character(1))
}

test_that("pandas really does hand back an all-character empty dictionary result", {
  skip_unless_python_module()

  frames <- dictionary_frames()

  expect_equal(nrow(frames$empty), 0L)
  expect_true(all(column_types(frames$empty) == "character"))
  expect_false(all(column_types(frames$full) == "character"))
})

test_that("the dictionary schema makes the empty and non-empty results agree", {
  skip_unless_python_module()

  frames <- dictionary_frames()
  typed_empty <- picsure:::apply_result_schema(frames$empty, picsure:::.DICTIONARY_RESULT_SCHEMA)
  typed_full  <- picsure:::apply_result_schema(frames$full, picsure:::.DICTIONARY_RESULT_SCHEMA)

  expect_identical(column_types(typed_empty), column_types(typed_full))
  expect_identical(
    unname(column_types(typed_empty)),
    c("character", "character", "character", "character", "character",
      "character", "list", "numeric", "numeric", "logical", "list", "character")
  )
  expect_equal(nrow(typed_empty), 0L)
  expect_equal(nrow(typed_full), 1L)
  expect_identical(names(typed_empty), names(frames$empty))
})

test_that("arithmetic on min / max works on an empty search result", {
  skip_unless_python_module()

  frames <- dictionary_frames()
  typed <- picsure:::apply_result_schema(frames$empty, picsure:::.DICTIONARY_RESULT_SCHEMA)

  expect_error(as.character(frames$empty$max) - as.character(frames$empty$min))
  expect_identical(typed$max - typed$min, numeric(0))
  expect_equal(sum(typed$max - typed$min), 0)
  expect_type(typed$allowFiltering, "logical")
  expect_identical(typed$allowFiltering, logical(0))
})

test_that("searchDictionary() types a real pandas result end to end", {
  skip_unless_python_module()

  frames <- dictionary_frames()
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  bdc$searchDictionary <- function(...) frames$empty

  empty_result <- picsure::searchDictionary(bdc, "no-such-term")

  bdc$searchDictionary <- function(...) frames$full
  full_result <- picsure::searchDictionary(bdc, "bmi")

  expect_equal(nrow(empty_result), 0L)
  expect_equal(nrow(full_result), 1L)
  expect_identical(column_types(empty_result), column_types(full_result))
  expect_type(empty_result$min, "double")
  expect_type(empty_result$allowFiltering, "logical")
})

test_that("a deployment-specific extra column survives the schema untouched", {
  skip_unless_python_module()

  reticulate::py_run_string("
import pandas as pd

_dict_extra = pd.DataFrame({
    'conceptPath': ['\\\\phs1\\\\bmi\\\\'],
    'min': ['12.5'],
    'someFutureField': ['keep me'],
})
")
  typed <- picsure:::apply_result_schema(
    reticulate::py$`_dict_extra`, picsure:::.DICTIONARY_RESULT_SCHEMA
  )

  expect_identical(names(typed), c("conceptPath", "min", "someFutureField"))
  expect_type(typed$min, "double")
  expect_equal(typed$someFutureField, "keep me")
})

test_that("the dictionary schema matches the pinned Python adapter's column list", {
  skip_unless_python_module("picsure._services.search")
  search <- reticulate::import("picsure._services.search")

  with_values    <- as.character(search$`_COLUMNS_WITH_VALUES`)
  without_values <- as.character(search$`_COLUMNS_WITHOUT_VALUES`)

  expect_identical(names(picsure:::.DICTIONARY_RESULT_SCHEMA), with_values)
  expect_identical(setdiff(with_values, without_values), "values")
})

test_that("the consequences schema matches the pinned Python adapter's column list", {
  skip_unless_python_module()
  consequences <- reticulate::import("picsure")$genomicConsequences()

  expect_s3_class(consequences, "data.frame")
  expect_gt(nrow(consequences), 0L)
  expect_identical(
    names(consequences), names(picsure:::.CONSEQUENCES_RESULT_SCHEMA),
    info = paste0(
      "genomicConsequences() in the pinned Python build returns columns ",
      paste(names(consequences), collapse = ", "),
      " while .CONSEQUENCES_RESULT_SCHEMA in R/utils_coerce.R declares ",
      paste(names(picsure:::.CONSEQUENCES_RESULT_SCHEMA), collapse = ", "),
      ". apply_result_schema() types only the columns both sides name, so a ",
      "renamed column silently stops being typed rather than failing. Bring ",
      "the schema back into line with the Python column list."
    )
  )
})
