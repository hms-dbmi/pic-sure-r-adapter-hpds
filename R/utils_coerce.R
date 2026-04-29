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

#' Resolve a string OR a typed enum member to its string identifier.
#'
#' Used by `to_py_enum()` and any other site that needs a string
#' representation of either a `picsure_enum_member` or a plain string.
#' Members are required to match `expected_subclass` to prevent passing
#' a `ClauseType` member where a `GroupOperator` is expected.
#'
#' @param value NULL, a single string, or a `picsure_enum_member`.
#' @param expected_subclass The required `picsure_*` subclass
#'   (e.g. `"picsure_clause_type"`).
#' @param what Human-readable enum name for error messages
#'   (e.g. `"ClauseType"`).
#' @param field For members, which field to extract: `"name"` (default)
#'   or `"value"`.
#' @return NULL if `value` is NULL; otherwise a character scalar.
#' @keywords internal
as_enum_string <- function(value, expected_subclass, what, field = "name") {
  if (is.null(value)) return(NULL)
  if (inherits(value, "picsure_enum_member")) {
    if (!inherits(value, expected_subclass)) {
      stop(sprintf(
        "Expected a %s member, got %s.", what, format(value)
      ))
    }
    return(value[[field]])
  }
  if (!is.character(value) || length(value) != 1L) {
    stop(sprintf(
      "%s value must be a single string or %s member.", what, what
    ))
  }
  value
}

#' Resolve a case-insensitive string OR a typed enum member against a
#' Python enum proxy.
#'
#' The R API accepts strings like "FILTER", "and", or members like
#' [`picsure::ClauseType$FILTER`][picsure::ClauseType] and maps them to
#' the Python enum member at call time.
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
  s <- as_enum_string(value, expected_subclass, enum_name, field = "name")
  if (is.null(s)) return(NULL)
  valid <- names(enum_obj)
  match_idx <- which(tolower(valid) == tolower(s))
  if (length(match_idx) == 0L) {
    stop(sprintf(
      "%s value '%s' is not one of: %s",
      enum_name, s, paste(valid, collapse = ", ")
    ))
  }
  enum_obj[[valid[match_idx]]]
}
