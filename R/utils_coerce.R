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

#' Resolve a case-insensitive string against a Python enum proxy.
#'
#' The R API accepts strings like "FILTER", "and", "bdc authorized" and maps
#' them to the Python enum member at call time. `enum_obj` is whatever
#' `picsure_py$ClauseType` (etc.) returns — in real use, a reticulate object
#' whose members are accessible via `$` and `names()`. In tests, a named list
#' stands in for it.
#'
#' @param value NULL or a length-1 character.
#' @param enum_obj The Python enum proxy (or a named list in tests).
#' @param enum_name The enum name, used only in error messages.
#' @return NULL if value is NULL; otherwise the corresponding enum member.
#' @keywords internal
to_py_enum <- function(value, enum_obj, enum_name) {
  if (is.null(value)) return(NULL)
  if (!is.character(value) || length(value) != 1L) {
    stop(enum_name, " value must be a single string, got: ", format(value))
  }
  valid <- names(enum_obj)
  match_idx <- which(tolower(valid) == tolower(value))
  if (length(match_idx) == 0L) {
    stop(sprintf(
      "%s value '%s' is not one of: %s",
      enum_name, value, paste(valid, collapse = ", ")
    ))
  }
  enum_obj[[valid[match_idx]]]
}
