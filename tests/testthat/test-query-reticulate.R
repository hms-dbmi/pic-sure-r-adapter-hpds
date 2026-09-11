# Timeseries-result typing, across the real reticulate boundary.
#
# The same root cause as the dictionary schema, in the other direction. HPDS's
# TimeseriesProcessor writes a fixed header —
# PATIENT_NUM, CONCEPT_PATH, NVAL_NUM, TVAL_CHAR, TIMESTAMP — and fills
# exactly one of NVAL_NUM / TVAL_CHAR per row: the number for a numeric
# concept, the text for a string concept, the other left empty. The Python
# adapter parses that with `pandas.read_csv`, which infers per column, so a
# query over numeric concepts alone leaves TVAL_CHAR empty in every row and
# read_csv types it float64. The text column then reached R as numeric,
# changing type with the data. Only real pandas parsing shows this, which is
# why it is tested here.

# The header TimeseriesProcessor.getHeaderRow() writes, verbatim.
TIMESERIES_HEADER <- "PATIENT_NUM,CONCEPT_PATH,NVAL_NUM,TVAL_CHAR,TIMESTAMP"

# Parses a timeseries CSV body exactly as the Python adapter's
# _parse_dataframe() does, and hands the result across to R.
parse_timeseries_csv <- function(rows) {
  reticulate::py_run_string("
import pandas as pd
from io import StringIO


def _parse_timeseries(text):
    return pd.read_csv(StringIO(text))
")
  reticulate::py$`_parse_timeseries`(paste0(paste(c(TIMESERIES_HEADER, rows),
                                                 collapse = "\n"), "\n"))
}

timeseries_column_types <- function(data) {
  vapply(data, function(column) class(column)[[1L]], character(1))
}

test_that("read_csv really does type an all-empty text column as numeric", {
  skip_unless_python_module()

  # A query over numeric concepts only: TVAL_CHAR is empty in every row.
  numeric_only <- parse_timeseries_csv(c(
    "1,\\\\phs1\\\\age\\\\,42.0,,2020-01-01T00:00:00Z",
    "2,\\\\phs1\\\\age\\\\,51.0,,2020-01-02T00:00:00Z"
  ))
  # A query over string concepts only: NVAL_NUM is empty in every row.
  text_only <- parse_timeseries_csv(c(
    "1,\\\\phs1\\\\sex\\\\,,F,2020-01-01T00:00:00Z"
  ))

  expect_equal(timeseries_column_types(numeric_only)[["TVAL_CHAR"]], "numeric")
  expect_equal(timeseries_column_types(text_only)[["TVAL_CHAR"]], "character")
  # The same column, two types, decided by the data. That is the defect.
  expect_false(identical(
    timeseries_column_types(numeric_only)[["TVAL_CHAR"]],
    timeseries_column_types(text_only)[["TVAL_CHAR"]]
  ))
})

test_that("the timeseries schema types TVAL_CHAR the same whatever the data", {
  skip_unless_python_module()

  frames <- list(
    numeric_only = parse_timeseries_csv(c("1,x,42.0,,2020-01-01T00:00:00Z")),
    text_only    = parse_timeseries_csv(c("1,x,,F,2020-01-01T00:00:00Z")),
    mixed        = parse_timeseries_csv(c("1,x,42.0,,2020-01-01T00:00:00Z",
                                          "1,y,,F,2020-01-02T00:00:00Z"))
  )

  expected <- c(
    PATIENT_NUM  = "integer",
    CONCEPT_PATH = "character",
    NVAL_NUM     = "numeric",
    TVAL_CHAR    = "character",
    TIMESTAMP    = "character"
  )
  for (name in names(frames)) {
    typed <- picsure:::apply_result_schema(frames[[name]], picsure:::.TIMESERIES_RESULT_SCHEMA)
    expect_identical(timeseries_column_types(typed), expected, info = name)
  }
})

test_that("runQuery(type = 'timestamp') types a real parsed result end to end", {
  skip_unless_python_module()

  numeric_only <- parse_timeseries_csv(c("1,x,42.0,,2020-01-01T00:00:00Z"))
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  bdc$runQuery <- function(...) numeric_only

  result <- picsure::runQuery(bdc, list(kind = "clause"), type = "timestamp")

  expect_type(result$TVAL_CHAR, "character")
  expect_type(result$NVAL_NUM, "double")
  expect_type(result$PATIENT_NUM, "integer")
})

test_that("the timestamp schema applies whether type came as a string or a member", {
  skip_unless_python_module()

  numeric_only <- parse_timeseries_csv(c("1,x,42.0,,2020-01-01T00:00:00Z"))
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  bdc$runQuery <- function(...) numeric_only
  bdc$runQueryByID <- function(...) numeric_only

  for (type in list("timestamp", "TIMESTAMP", picsure::QueryType$TIMESTAMP)) {
    expect_type(picsure::runQuery(bdc, list(kind = "clause"), type = type)$TVAL_CHAR,
                "character")
    expect_type(picsure::runQueryByID(bdc, "an-id", type = type)$TVAL_CHAR,
                "character")
  }
})

test_that("a participant result's server-driven columns are left alone", {
  skip_unless_python_module()

  # Participant and VCF-excerpt results have one column per concept the query
  # asked for, so there is no schema to apply and inference is all there is.
  reticulate::py_run_string("
import pandas as pd
from io import StringIO

_participant = pd.read_csv(StringIO(
    'PATIENT_NUM,\\\\phs1\\\\bmi\\\\,\\\\phs1\\\\sex\\\\\\n1,27.4,F\\n'
))
")
  participant <- reticulate::py$`_participant`
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "https://picsure.test", token = "tok")
  bdc$runQuery <- function(...) participant

  result <- picsure::runQuery(bdc, list(kind = "clause"), type = "participant")

  expect_identical(names(result), names(participant))
  expect_identical(
    vapply(result, function(column) class(column)[[1L]], character(1)),
    vapply(participant, function(column) class(column)[[1L]], character(1))
  )
})
