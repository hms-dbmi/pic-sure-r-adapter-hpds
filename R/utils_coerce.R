# Helpers for converting between R and Python types when calling into the
# Python `picsure` module via reticulate. Each helper targets one concrete
# coercion concern; wrappers in connect.R, search.R, clauses.R, query.R,
# and export.R compose them before handing args to Python.

#' Drop NULL entries from a list.
#'
#' Used to normalize keyword-argument lists before handing them to Python:
#' reticulate maps R NULL to Python None, which bypasses Python defaults on
#' optional args. Drop NULLs first so Python sees its own defaults.
#'
#' @param x A list.
#' @return A list with all NULL-valued entries removed.
#' @keywords internal
drop_nulls <- function(x) {
  out <- x[!vapply(x, is.null, logical(1))]
  if (length(out) == 0L) list() else out
}

#' Render a rejected argument value for an error message.
#'
#' Says what actually arrived, so "`page` must be a single positive whole
#' number" is followed by the value that failed rather than leaving the
#' caller to guess which of several arguments was wrong.
#'
#' @param value The rejected value.
#' @return A character scalar describing `value`.
#' @keywords internal
describe_argument_value <- function(value) {
  if (is.null(value)) {
    return("NULL")
  }
  if (length(value) == 0L) {
    return(sprintf("an empty %s vector", class(value)[[1L]]))
  }
  if (length(value) > 1L) {
    shown <- utils::head(vapply(value, function(v) paste(format(v), collapse = " "),
                                character(1), USE.NAMES = FALSE), 4L)
    return(sprintf(
      "%d values (%s%s)", length(value), paste(shown, collapse = ", "),
      if (length(value) > length(shown)) ", ..." else ""
    ))
  }
  if (is.character(value)) {
    return(sprintf("the string %s", encodeString(value, quote = "\"")))
  }
  if (is.na(value)) {
    return(sprintf("%s NA", class(value)[[1L]]))
  }
  sprintf("%s (%s)", format(value), class(value)[[1L]])
}

#' Validate a single positive whole number.
#'
#' Page and size arguments used to go through `as.integer()` with no check at
#' all, so `page = 1.7` was silently truncated to 1 and `page = "two"` became
#' `NA` and was forwarded to Python. Each rejection names the argument and
#' what arrived, and raises a `picsureValidationError` (a `picsureError`).
#'
#' @param value The value to validate.
#' @param arg The argument's name, for the error message.
#' @return `value` as an integer scalar.
#' @keywords internal
as_positive_whole_number <- function(value, arg) {
  reject <- function(requirement) {
    stop(.picsure_invalid_argument(
      sprintf("`%s` must be %s; got %s.", arg, requirement,
              describe_argument_value(value)),
      call = NULL
    ))
  }
  if (is.null(value) || length(value) != 1L || !is.numeric(value)) {
    reject("a single positive whole number")
  }
  if (is.na(value)) {
    reject("a single positive whole number")
  }
  if (!is.finite(value)) {
    reject("a finite number")
  }
  if (value != trunc(value)) {
    reject("a whole number, with no fractional part")
  }
  if (value < 1) {
    reject("a positive number, 1 or greater")
  }
  if (value > .Machine$integer.max) {
    reject(sprintf("at most %d", .Machine$integer.max))
  }
  as.integer(value)
}

#' Validate an optional single finite number, leaving its type alone.
#'
#' For continuous bounds, where a negative or fractional value is legitimate
#' but `NA`, `Inf`, a string, and a vector are not. `NULL` passes through so
#' the Python default applies. The value is returned unchanged rather than
#' coerced: an R integer must stay an integer so reticulate hands Python an
#' `int` rather than a `float`.
#'
#' @param value The value to validate, or NULL.
#' @param arg The argument's name, for the error message.
#' @return `value` unchanged.
#' @keywords internal
check_optional_number <- function(value, arg) {
  if (is.null(value)) {
    return(NULL)
  }
  reject <- function(requirement) {
    stop(.picsure_invalid_argument(
      sprintf("`%s` must be %s; got %s.", arg, requirement,
              describe_argument_value(value)),
      call = NULL
    ))
  }
  if (length(value) != 1L || !is.numeric(value)) {
    reject("a single number, or NULL")
  }
  if (is.na(value) || !is.finite(value)) {
    reject("a single finite number, or NULL")
  }
  value
}

#' Validate a single non-empty string.
#'
#' Checks length before content, so a vector argument raises a package error
#' naming the argument rather than base R's "'length = 2' in coercion to
#' 'logical(1)'" from an `if()` handed a vector.
#'
#' @param value The value to validate.
#' @param arg The argument's name, for the error message.
#' @param hint Optional sentence appended to the error message.
#' @return `value` unchanged.
#' @keywords internal
as_single_string <- function(value, arg, hint = NULL) {
  reject <- function(requirement) {
    stop(.picsure_invalid_argument(
      paste0(
        sprintf("`%s` must be %s; got %s.", arg, requirement,
                describe_argument_value(value)),
        if (is.null(hint)) "" else paste0(" ", hint)
      ),
      call = NULL
    ))
  }
  if (is.null(value) || length(value) != 1L || !is.character(value)) {
    reject("a single non-empty character string")
  }
  if (is.na(value) || !nzchar(value)) {
    reject("a single non-empty character string")
  }
  value
}

#' Resolve a string OR a typed enum member to its string identifier.
#'
#' Used by `to_py_enum()` and any other site that needs a string
#' representation of either a `picsure_enum_member` or a plain string.
#' Members are required to match `expected_subclass` to prevent passing
#' a `PhenotypicFilterType` member where a `GroupOperator` is expected.
#'
#' @param value NULL, a single string, or a `picsure_enum_member`.
#' @param expected_subclass The required `picsure_*` subclass
#'   (e.g. `"picsure_phenotypic_filter_type"`).
#' @param enum_name Human-readable enum name for error messages
#'   (e.g. `"PhenotypicFilterType"`).
#' @param field For members, which field to extract: `"name"` (default)
#'   or `"value"`.
#' @return NULL if `value` is NULL; otherwise a character scalar.
#' @keywords internal
as_enum_string <- function(value, expected_subclass, enum_name, field = "name") {
  if (is.null(value)) return(NULL)
  if (inherits(value, "picsure_enum_member")) {
    if (!inherits(value, expected_subclass)) {
      stop(picsureError(
        sprintf("Expected a %s member, got %s.", enum_name, format(value)),
        class = "picsureValidationError"
      ))
    }
    return(value[[field]])
  }
  if (!is.character(value) || length(value) != 1L) {
    stop(.picsure_invalid_argument(
      sprintf(
        "%s value must be a single string or %s member; got %s.",
        enum_name, enum_name, describe_argument_value(value)
      ),
      call = NULL
    ))
  }
  value
}

# Lists the member names of a Python enum class, or of the named R list that
# stands in for one in the unit tests.
#
# `names()` on a Python enum class is NOT the member list: it reports every
# attribute. For an enum that mixes in `str` — VariantFrequency,
# GenomicFilterKey, and VariantSeverity all do — that includes all 47 string
# methods, so `count`, `index`, `format`, and `strip` look like members. Only
# `__members__` is the member map.
#
# `__members__` is a mappingproxy, and reticulate has no converter for that
# type, so it is copied into a real dict first: that gives `py_to_r()`
# something it knows how to convert.
.py_enum_member_names <- function(enum_obj) {
  if (!inherits(enum_obj, "python.builtin.object")) {
    return(names(enum_obj))
  }
  mapping <- enum_obj$`__members__`
  if (inherits(mapping, "python.builtin.object")) {
    builtins <- reticulate::import_builtins()
    mapping <- reticulate::py_to_r(builtins$dict(mapping))
  }
  names(mapping)
}

# Fetches one member of a Python enum class by exact member name.
#
# Goes through `__members__[name]`, never attribute access: for a str-mixin
# enum, `enum_obj$count` is the bound `str.count` method, so a member whose
# name collided with a string method would resolve to the method instead of
# the member. `__members__` holds members only, and raises KeyError for
# anything else.
.py_enum_member <- function(enum_obj, name) {
  if (!inherits(enum_obj, "python.builtin.object")) {
    return(enum_obj[[name]])
  }
  enum_obj$`__members__`[[name]]
}

#' Resolve a case-insensitive string OR a typed enum member against a
#' Python enum proxy.
#'
#' The R API accepts strings like "FILTER", "and", or members like
#' [`picsure::PhenotypicFilterType$FILTER`][picsure::PhenotypicFilterType]
#' and maps them to the Python enum member at call time. Resolution goes
#' through the enum's `__members__` map, so only real members can be
#' returned.
#'
#' @param value NULL, a single string, or a `picsure_enum_member`.
#' @param enum_obj The Python enum proxy (or a named list in tests).
#' @param enum_name The enum name, used only in error messages.
#' @param expected_subclass The required `picsure_*` subclass for member
#'   inputs. Members of other subclasses are rejected before any proxy
#'   lookup happens.
#' @return NULL if value is NULL; otherwise the corresponding enum member.
#' @keywords internal
to_py_enum <- function(value, enum_obj, enum_name, expected_subclass) {
  s <- as_enum_string(value, expected_subclass, enum_name = enum_name, field = "name")
  # as_enum_string returns NULL for NULL value; skip the proxy lookup.
  if (is.null(s)) return(NULL)
  valid <- .py_enum_member_names(enum_obj)
  match_idx <- which(valid == s)
  if (length(match_idx) == 0L) {
    match_idx <- which(tolower(valid) == tolower(s))
  }
  if (length(match_idx) == 0L) {
    stop(.picsure_invalid_argument(
      sprintf(
        "%s value '%s' is not one of: %s",
        enum_name, s, paste(valid, collapse = ", ")
      ),
      call = NULL
    ))
  }
  if (length(match_idx) > 1L) {
    stop(.picsure_invalid_argument(
      sprintf(
        "%s value '%s' matches %d members case-insensitively (%s); pass the exact member name.",
        enum_name, s, length(match_idx), paste(valid[match_idx], collapse = ", ")
      ),
      call = NULL
    ))
  }
  .py_enum_member(enum_obj, valid[[match_idx]])
}

# Column types of a dictionary-search result.
#
# The Python adapter builds the empty result as `pd.DataFrame(columns=...)`,
# whose columns are all dtype `object`, so a search that matched nothing
# reached R with every column character — including `min`, `max`, and
# `allowFiltering`, which are numeric and logical on any non-empty result.
# Downstream arithmetic therefore broke only when the search found nothing.
# Typing the result from the schema instead of from the rows it happens to
# contain makes the two cases agree.
#
# Types come from `DictionaryEntry` in the Python adapter: six string fields,
# a categorical value list, two optional floats, an optional bool, an
# optional metadata dict, and an optional string. `values` and `meta` are
# list columns in both directions, since each cell holds a vector or a
# mapping rather than a scalar.
.DICTIONARY_RESULT_SCHEMA <- c(
  conceptPath    = "character",
  name           = "character",
  display        = "character",
  description    = "character",
  dataType       = "character",
  studyId        = "character",
  values         = "list",
  min            = "numeric",
  max            = "numeric",
  allowFiltering = "logical",
  meta           = "list",
  studyAcronym   = "character"
)

# Column types of a timeseries (`type = "timestamp"`) query result.
#
# HPDS's TimeseriesProcessor writes a fixed header and fills exactly one of
# NVAL_NUM / TVAL_CHAR per row: the numeric value for a numeric concept and
# the text value for a string concept, with the other left empty. A query
# over numeric concepts alone therefore produces an all-empty TVAL_CHAR
# column, which `pandas.read_csv` infers as float64 — so a text column
# arrived in R as numeric depending on which concepts the query touched.
.TIMESERIES_RESULT_SCHEMA <- c(
  PATIENT_NUM  = "integer",
  CONCEPT_PATH = "character",
  NVAL_NUM     = "numeric",
  TVAL_CHAR    = "character",
  TIMESTAMP    = "character"
)

# Column types of a genomic value-search result.
.GENOMIC_VALUES_RESULT_SCHEMA <- c(value = "character")

# Column types of the offline variant-consequence vocabulary.
.CONSEQUENCES_RESULT_SCHEMA <- c(severity = "character", consequence = "character")

#' Type a result data frame from its known schema.
#'
#' Applies a declared column type to every column the schema names and the
#' frame actually has. Columns the schema does not name are left alone, so a
#' server-driven column set (a participant result's concept-path columns, or
#' a deployment-specific dictionary field) survives untouched, and no column
#' is added, dropped, or reordered.
#'
#' @param data A data frame, or any other value (returned unchanged).
#' @param schema A named character vector mapping column name to one of
#'   `"character"`, `"numeric"`, `"integer"`, `"logical"`, or `"list"`.
#' @return `data` with the named columns retyped.
#' @keywords internal
apply_result_schema <- function(data, schema) {
  if (!is.data.frame(data)) {
    return(data)
  }
  for (column in intersect(names(schema), names(data))) {
    data[[column]] <- coerce_result_column(data[[column]], schema[[column]])
  }
  data
}

#' Coerce one result column to a declared type.
#'
#' A list column reaching a scalar type means the server sent a nested value
#' where the schema expects one per row. Those cells are collapsed to a single
#' string rather than deparsed, which is what `as.character()` on a list
#' would do.
#'
#' @param column The column as it arrived.
#' @param type One of `"character"`, `"numeric"`, `"integer"`, `"logical"`,
#'   `"list"`.
#' @return The column, coerced.
#' @keywords internal
coerce_result_column <- function(column, type) {
  if (identical(type, "list")) {
    return(if (is.list(column)) column else as.list(column))
  }
  if (is.list(column)) {
    column <- vapply(
      column,
      function(cell) {
        if (length(cell) == 0L) NA_character_ else paste(as.character(cell), collapse = ", ")
      },
      character(1), USE.NAMES = FALSE
    )
  }
  switch(
    type,
    character = as.character(column),
    numeric   = suppressWarnings(as.numeric(column)),
    integer   = suppressWarnings(as.integer(column)),
    logical   = as.logical(column),
    column
  )
}
