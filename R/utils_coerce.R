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
