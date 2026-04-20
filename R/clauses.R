#' Create a single query clause.
#'
#' Builds an opaque Clause handle suitable for nesting inside a
#' [`buildClauseGroup()`][picsure::buildClauseGroup] tree and running via
#' [`runQuery()`][picsure::runQuery].
#'
#' @param path The HPDS concept path the clause applies to,
#'   e.g. `"\\phs000001\\pht000001\\phv00000001\\sex\\"`.
#' @param type Clause type as a case-insensitive string; one of
#'   `"FILTER"`, `"SELECT"`, `"REQUIRE"`, `"ANYRECORD"`.
#' @param min,max Optional numeric bounds for continuous FILTER clauses.
#'   Coerced to Python `int` when integral.
#' @param categories Optional list of accepted category values for
#'   categorical FILTER clauses.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `picsure.createClause()` call.
#' @return An opaque Clause handle.
#' @examples
#' \dontrun{
#' sex <- picsure::createClause(
#'   "\\phs1\\pht1\\phv1\\sex\\",
#'   type = "FILTER", categories = list("male")
#' )
#' }
#' @export
createClause <- function(path, type, min = NULL, max = NULL, categories = NULL, ...) {
  if (missing(path) || is.null(path) || is.na(path) || !nzchar(path)) {
    stop("`path` is required.")
  }
  if (missing(type) || is.null(type)) {
    stop("`type` is required.")
  }

  kwargs <- drop_nulls(list(
    path       = path,
    type       = to_py_enum(type, picsure_py$ClauseType, "ClauseType"),
    min        = as_py_int(min),
    max        = as_py_int(max),
    categories = categories,
    ...
  ))

  with_picsure_error(do.call(picsure_py$createClause, kwargs))
}

#' Combine clauses (and nested groups) under an AND or OR operator.
#'
#' Takes a list of clause / group handles and returns a single opaque
#' ClauseGroup that can itself be nested inside another
#' `buildClauseGroup()` call, or passed to
#' [`runQuery()`][picsure::runQuery].
#'
#' @param clauses A non-empty list of clause or clause-group handles.
#' @param root The root operator, a case-insensitive string; one of
#'   `"AND"` or `"OR"`. Defaults to `"AND"`.
#' @return An opaque ClauseGroup handle.
#' @examples
#' \dontrun{
#' sex    <- picsure::createClause("\\phs1\\pht1\\phv1\\sex\\", type = "FILTER", categories = list("male"))
#' copd   <- picsure::createClause("\\phs1\\pht2\\phv2\\copd\\", type = "FILTER", categories = list("Yes"))
#' asthma <- picsure::createClause("\\phs1\\pht2\\phv3\\asth\\", type = "FILTER", categories = list("Yes"))
#' lung <- picsure::buildClauseGroup(list(copd, asthma), root = "OR")
#' full <- picsure::buildClauseGroup(list(sex, lung), root = "AND")
#' }
#' @export
buildClauseGroup <- function(clauses, root = "AND") {
  if (missing(clauses) || !is.list(clauses) || length(clauses) == 0L) {
    stop("`clauses` must be a non-empty list of clause or clause-group handles.")
  }

  with_picsure_error(picsure_py$buildClauseGroup(
    clauses = clauses,
    root    = to_py_enum(root, picsure_py$GroupOperator, "GroupOperator")
  ))
}
