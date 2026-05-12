#' Create a single query clause.
#'
#' Builds an opaque Clause handle suitable for nesting inside a
#' [`buildClauseGroup()`][picsure::buildClauseGroup] tree and running via
#' [`runQuery()`][picsure::runQuery].
#'
#' @param keys One or more HPDS concept paths the clause applies to. A
#'   character scalar or character vector, e.g.
#'   `"\\phs000001\\pht000001\\phv00000001\\sex\\"` or
#'   `c("\\path\\a\\", "\\path\\b\\")`.
#' @param type Clause type. A case-insensitive string (one of `"FILTER"`,
#'   `"SELECT"`, `"REQUIRE"`, `"ANYRECORD"`) or a `ClauseType` member
#'   (e.g. [`ClauseType$FILTER`][picsure::ClauseType]).
#' @param min,max Optional numeric bounds for continuous FILTER clauses.
#' @param categories Optional vector or list of accepted category values for
#'   categorical FILTER clauses.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `picsure.createClause()` call.
#' @return An opaque Clause handle.
#' @examples
#' \dontrun{
#' sex <- picsure::createClause(
#'   "\\phs1\\pht1\\phv1\\sex\\",
#'   type = "FILTER", categories = "male"
#' )
#' }
#' @export
createClause <- function(keys, type, min = NULL, max = NULL, categories = NULL, ...) {
  if (missing(keys) || is.null(keys) || length(keys) == 0L ||
      !is.character(keys) || any(is.na(keys)) || any(!nzchar(keys))) {
    stop("`keys` must be a non-empty character string or vector.")
  }
  if (missing(type) || is.null(type)) {
    stop("`type` is required. One of \"FILTER\", \"SELECT\", \"REQUIRE\", \"ANYRECORD\" (case-insensitive).")
  }

  kwargs <- drop_nulls(list(
    keys       = keys,
    type       = to_py_enum(type, picsure_py$ClauseType, "ClauseType", "picsure_clause_type"),
    min        = min,
    max        = max,
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
#' @param operator The group operator. A case-insensitive string (`"AND"`
#'   or `"OR"`) or a `GroupOperator` member (e.g.
#'   [`GroupOperator$AND`][picsure::GroupOperator]). Defaults to `"AND"`.
#' @return An opaque ClauseGroup handle.
#' @examples
#' \dontrun{
#' sex    <- picsure::createClause("\\phs1\\pht1\\phv1\\sex\\", type = "FILTER", categories = "male")
#' copd   <- picsure::createClause("\\phs1\\pht2\\phv2\\copd\\", type = "FILTER", categories = "Yes")
#' asthma <- picsure::createClause("\\phs1\\pht2\\phv3\\asth\\", type = "FILTER", categories = "Yes")
#' lung <- picsure::buildClauseGroup(list(copd, asthma), operator = "OR")
#' full <- picsure::buildClauseGroup(list(sex, lung), operator = "AND")
#' }
#' @export
buildClauseGroup <- function(clauses, operator = "AND") {
  if (missing(clauses) || !is.list(clauses) || length(clauses) == 0L) {
    stop("`clauses` must be a non-empty list of clause or clause-group handles.")
  }

  with_picsure_error(picsure_py$buildClauseGroup(
    clauses = clauses,
    operator = to_py_enum(operator, picsure_py$GroupOperator, "GroupOperator", "picsure_group_operator")
  ))
}
