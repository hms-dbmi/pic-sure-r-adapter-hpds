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
#' Returns one string for any value, atomic or not. A one-column data frame
#' and a function both arrive here with length 1, and each breaks a scalar
#' result if treated as atomic: `is.na()` on a data frame returns one row per
#' row of the frame, and `format()` on a function returns one string per
#' deparsed line. Either way the rejection escapes as a base R
#' condition-length error, or as a condition whose message is a vector, and
#' the argument that was actually wrong goes unnamed.
#'
#' @param value The rejected value, of any type.
#' @return A character scalar describing `value`, including for a non-atomic
#'   value such as a data frame, a list, or a function.
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
  if (is.atomic(value) && is.na(value)) {
    return(sprintf("%s NA", class(value)[[1L]]))
  }
  sprintf("%s (%s)", paste(format(value), collapse = " "), class(value)[[1L]])
}

#' Validate a single positive whole number.
#'
#' Rejects `NA`, `NaN`, `Inf`, a fraction, anything below 1, anything above
#' `.Machine$integer.max`, non-numeric input, and any length other than one,
#' so `page = 1.7` is an error rather than a silent truncation to 1. Each
#' rejection names the argument and what arrived, and raises a
#' `picsureValidationError`, which is a `picsureError`.
#'
#' @param value The value to validate.
#' @param arg The argument's name, for the error message.
#' @return `value` as an integer scalar.
#' @keywords internal
as_positive_whole_number <- function(value, arg) {
  reject <- function(requirement) {
    .picsure_reject(
      sprintf("`%s` must be %s; got %s.", arg, requirement,
              describe_argument_value(value)),
      call = NULL
    )
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
    .picsure_reject(
      sprintf("`%s` must be %s; got %s.", arg, requirement,
              describe_argument_value(value)),
      call = NULL
    )
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
    .picsure_reject(
      paste0(
        sprintf("`%s` must be %s; got %s.", arg, requirement,
                describe_argument_value(value)),
        if (is.null(hint)) "" else paste0(" ", hint)
      ),
      call = NULL
    )
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
      .picsure_reject(
        sprintf("Expected a %s member, got %s.", enum_name, format(value)),
        call = NULL
      )
    }
    return(value[[field]])
  }
  if (!is.character(value) || length(value) != 1L) {
    .picsure_reject(
      sprintf(
        "%s value must be a single string or %s member; got %s.",
        enum_name, enum_name, describe_argument_value(value)
      ),
      call = NULL
    )
  }
  value
}

#' Convert a Python enum class's member map into a named R list.
#'
#' `names()` on a Python enum class is not the member list. It reports every
#' attribute, and for an enum that mixes in `str` that includes all 47 string
#' methods, so `count`, `index`, `format`, and `strip` look like members.
#' VariantFrequency, GenomicFilterKey, and VariantSeverity all mix in `str`.
#' Only `__members__` is the member map. It is a mappingproxy, which
#' reticulate cannot convert: `py_to_r()` hands the proxy back unchanged and
#' iterating it from R fails with "cannot coerce type 'environment' to vector
#' of type 'list'". Copying it into a real dict first gives reticulate a type
#' it does convert.
#'
#' @param enum_obj The Python enum class. Anything that is not a Python
#'   object (the named list or character vector a unit test stands in with)
#'   is returned untouched.
#' @return A named list of members, or the non-Python input unchanged.
#' @noRd
.py_enum_members <- function(enum_obj) {
  if (!inherits(enum_obj, "python.builtin.object")) {
    return(enum_obj)
  }
  mapping <- enum_obj$`__members__`
  if (inherits(mapping, "python.builtin.object")) {
    builtins <- reticulate::import_builtins()
    mapping <- reticulate::py_to_r(builtins$dict(mapping))
  }
  as.list(mapping)
}

#' List the member names of a Python enum class.
#'
#' @param enum_obj The Python enum class, or the named R list that stands in
#'   for one in the unit tests.
#' @return A character vector of member names.
#' @noRd
.py_enum_member_names <- function(enum_obj) {
  names(.py_enum_members(enum_obj))
}

#' Fetch one member of a Python enum class by exact member name.
#'
#' Goes through `__members__[name]`, never attribute access. For a str-mixin
#' enum, `enum_obj$count` is the bound `str.count` method, so a member whose
#' name collided with a string method would resolve to the method instead of
#' the member. `__members__` holds members only and raises KeyError for
#' anything else.
#'
#' @param enum_obj The Python enum class, or the named R list that stands in
#'   for one in the unit tests.
#' @param name The exact member name.
#' @return The member.
#' @noRd
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
    .picsure_reject(
      sprintf(
        "%s value '%s' is not one of: %s",
        enum_name, s, paste(valid, collapse = ", ")
      ),
      call = NULL
    )
  }
  if (length(match_idx) > 1L) {
    .picsure_reject(
      sprintf(
        "%s value '%s' matches %d members case-insensitively (%s); pass the exact member name.",
        enum_name, s, length(match_idx), paste(valid[match_idx], collapse = ", ")
      ),
      call = NULL
    )
  }
  .py_enum_member(enum_obj, valid[[match_idx]])
}

#' Column types of a dictionary-search result.
#'
#' The Python adapter builds a matched-nothing result as
#' `pd.DataFrame(columns=...)`, whose columns are all dtype `object`, so an
#' empty search reached R with every column character, including `min`,
#' `max`, and `allowFiltering`, which are numeric and logical on any
#' non-empty result. Typing from this schema instead of from the rows makes
#' the two cases agree.
#'
#' Types follow `DictionaryEntry` in the Python adapter. `values` and `meta`
#' are list columns because each cell holds a vector or a mapping.
#' @noRd
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

#' Column types of a timeseries (`type = "timestamp"`) query result.
#'
#' HPDS's TimeseriesProcessor writes a fixed header and fills exactly one of
#' NVAL_NUM and TVAL_CHAR per row, leaving the other empty. A query over
#' numeric concepts alone therefore produces an all-empty TVAL_CHAR column,
#' which `pandas.read_csv` infers as float64, so without this schema a text
#' column arrived in R as numeric or character depending on the query.
#' @noRd
.TIMESERIES_RESULT_SCHEMA <- c(
  PATIENT_NUM  = "integer",
  CONCEPT_PATH = "character",
  NVAL_NUM     = "numeric",
  TVAL_CHAR    = "character",
  TIMESTAMP    = "character"
)

#' Column types of a genomic value-search result.
#'
#' This one mirrors an inline Python literal, not a module constant. The
#' pinned adapter builds the frame as `pd.DataFrame({"value": [...]})` at the
#' end of `search_genomic_values()` in
#' `picsure/_services/genomic_search.py`, so there is no importable name to
#' compare against and the only call that produces the frame goes over the
#' network. No live drift guard is possible, unlike the dictionary and
#' consequence schemas, which `test-search-reticulate.R` checks against the
#' pinned build. Read that Python function by hand when the pin moves: a
#' renamed column would leave `apply_result_schema()` typing nothing, with no
#' warning and no failing test.
#' @noRd
.GENOMIC_VALUES_RESULT_SCHEMA <- c(value = "character")

#' Column types of the offline variant-consequence vocabulary.
#'
#' Guarded against the pinned build by `test-search-reticulate.R`, which can
#' call `genomicConsequences()` directly because it reads bundled data and
#' needs no session.
#' @noRd
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
    data[[column]] <- coerce_result_column(data[[column]], schema[[column]], column)
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
#' A value that does not parse as the declared type becomes `NA`. The result
#' is still returned, and one warning per column names the column and how
#' many values were lost, so a server that starts sending text where the
#' schema expects a number is visible without breaking the call.
#'
#' An empty cell in a column the schema declares `"character"` reaches R as a
#' floating-point `NaN`: `pandas.read_csv` infers an all-empty column as
#' `float64`, and reticulate hands that across as a `numeric`. Those cells
#' become `NA_character_` rather than the three-character string `"NaN"` that
#' `as.character()` alone would produce, so a timeseries `TVAL_CHAR` reads as
#' missing whether or not the rows happened to carry any text. Nothing is
#' reported as lost, because `NaN` already satisfies `is.na()`.
#'
#' @param column The column as it arrived.
#' @param type One of `"character"`, `"numeric"`, `"integer"`, `"logical"`,
#'   `"list"`.
#' @param name The column's name, used in the warning.
#' @return The column, coerced.
#' @keywords internal
coerce_result_column <- function(column, type, name = "<unnamed>") {
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
  if (identical(type, "character") && is.double(column)) {
    column[is.nan(column)] <- NA_real_
  }
  coerced <- switch(
    type,
    character = as.character(column),
    numeric   = suppressWarnings(as.numeric(column)),
    integer   = suppressWarnings(as.integer(column)),
    logical   = as.logical(column),
    column
  )
  lost <- sum(is.na(coerced) & !is.na(column))
  if (lost > 0L) {
    warning(sprintf(
      "Column `%s` could not be fully coerced to %s: %d value%s became NA.",
      name, type, lost, if (lost == 1L) "" else "s"
    ), call. = FALSE)
  }
  coerced
}
