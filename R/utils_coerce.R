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

#' Convert NA to NULL; pass everything else through unchanged.
#'
#' Callers that want "optional scalar, defaulting in Python" pipe their
#' argument through `na_to_none()` then drop NULLs via `drop_nulls()`.
#'
#' @param x A length-1 value or NULL.
#' @return `NULL` if `x` is NA, otherwise `x`.
#' @keywords internal
na_to_none <- function(x) {
  if (is.null(x)) return(NULL)
  if (length(x) == 1L && is.na(x)) return(NULL)
  x
}

#' Coerce a value to a 1L integer, the way Python expects.
#'
#' R defaults to double; passing a double to a Python function that expects an
#' int (e.g., `limit`, `offset`) leads to type errors on the Python side.
#' Preserves NULL/NA so callers can pipe through drop_nulls().
#'
#' @param x NULL, NA, or a length-1 numeric value.
#' @return NULL if x is NULL/NA, otherwise an integer.
#' @keywords internal
as_py_int <- function(x) {
  x <- na_to_none(x)
  if (is.null(x)) return(NULL)
  if (is.integer(x)) return(x)
  if (is.numeric(x) && x == as.integer(x)) return(as.integer(x))
  stop("Expected an integer value, got: ", format(x))
}

#' Normalize an R value to an R list (reticulate maps lists to Python lists).
#'
#' Accepts atomic vectors, lists, or a single scalar; preserves NULL.
#'
#' @param x NULL, an atomic vector, a list, or a scalar.
#' @return NULL if x is NULL, otherwise an unnamed list.
#' @keywords internal
as_py_list <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x)) return(unname(x))
  if (is.atomic(x)) return(as.list(x))
  list(x)
}

#' Require a named list (so reticulate converts it to a Python dict).
#'
#' Categorical filters and similar kwargs must arrive as dicts, not lists.
#'
#' @param x NULL or a named list.
#' @return NULL if x is NULL, otherwise x.
#' @keywords internal
as_py_dict <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.list(x) || is.null(names(x)) || any(names(x) == "")) {
    stop("as_py_dict() requires a fully-named list")
  }
  x
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
