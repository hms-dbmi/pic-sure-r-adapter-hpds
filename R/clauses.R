#' Create a single query clause.
#'
#' Builds an opaque Clause handle suitable for nesting inside a
#' [`buildClauseGroup()`][picsure::buildClauseGroup] tree, assembling into a
#' query with [`buildQuery()`][picsure::buildQuery], or running via
#' [`runQuery()`][picsure::runQuery].
#'
#' @param keys One or more HPDS concept paths the clause applies to. A
#'   character scalar or character vector, e.g.
#'   `"\\phs000001\\pht000001\\phv00000001\\sex\\"` or
#'   `c("\\path\\a\\", "\\path\\b\\")`.
#' @param type Clause type. A case-insensitive string (one of `"FILTER"`,
#'   `"REQUIRE"`, `"ANYRECORD"`) or a `PhenotypicFilterType` member (e.g.
#'   [`PhenotypicFilterType$FILTER`][picsure::PhenotypicFilterType]).
#' @param min,max Optional numeric bounds for continuous FILTER clauses.
#' @param categories Optional vector or list of accepted category values for
#'   categorical FILTER clauses.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `picsure.buildClause()` call.
#' @return An opaque Clause handle.
#'
#' @details
#' Variables you filter on are returned as output columns automatically. To
#' include *additional* concept paths in query output without filtering, pass
#' them to [`buildQuery()`][picsure::buildQuery]'s `includeConcepts` argument —
#' output columns are not a clause type.
#' @examples
#' \dontrun{
#' sex <- picsure::buildClause(
#'   "\\phs1\\pht1\\phv1\\sex\\",
#'   type = "FILTER", categories = "male"
#' )
#' }
#' @export
buildClause <- function(keys, type, min = NULL, max = NULL, categories = NULL, ...) {
  if (missing(keys) || is.null(keys) || length(keys) == 0L ||
      !is.character(keys) || any(is.na(keys)) || any(!nzchar(keys))) {
    stop("`keys` must be a non-empty character string or vector.")
  }
  if (missing(type) || is.null(type)) {
    stop("`type` is required. One of \"FILTER\", \"REQUIRE\", \"ANYRECORD\" (case-insensitive).")
  }

  kwargs <- drop_nulls(list(
    keys       = keys,
    type       = to_py_enum(type, picsure_py$PhenotypicFilterType, "PhenotypicFilterType", "picsure_phenotypic_filter_type"),
    min        = min,
    max        = max,
    categories = categories,
    ...
  ))

  with_picsure_error(do.call(picsure_py$buildClause, kwargs))
}

#' Combine clauses (and nested groups) under an AND or OR operator.
#'
#' Takes a list of clause / group handles and returns a single opaque
#' ClauseGroup that can itself be nested inside another
#' `buildClauseGroup()` call, assembled into a query with
#' [`buildQuery()`][picsure::buildQuery], or passed to
#' [`runQuery()`][picsure::runQuery].
#'
#' @param clauses A non-empty list of clause or clause-group handles.
#' @param operator The group operator. A case-insensitive string (`"AND"`
#'   or `"OR"`) or a `GroupOperator` member (e.g.
#'   [`GroupOperator$AND`][picsure::GroupOperator]). Defaults to `"AND"`.
#' @return An opaque ClauseGroup handle.
#' @examples
#' \dontrun{
#' sex    <- picsure::buildClause("\\phs1\\pht1\\phv1\\sex\\", type = "FILTER", categories = "male")
#' copd   <- picsure::buildClause("\\phs1\\pht2\\phv2\\copd\\", type = "FILTER", categories = "Yes")
#' asthma <- picsure::buildClause("\\phs1\\pht2\\phv3\\asth\\", type = "FILTER", categories = "Yes")
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

#' Assemble a complete query from a filter tree and/or output concepts.
#'
#' Bundles a phenotypic filter (a clause or clause-group handle) with the
#' concept paths to include as output columns into a single Query handle that
#' can be passed to [`runQuery()`][picsure::runQuery],
#' [`exportAsPFB()`][picsure::exportAsPFB], or
#' [`saveQueryByName()`][picsure::saveQueryByName].
#'
#' @param phenotypicFilter A clause or clause-group handle (from
#'   [`buildClause()`][picsure::buildClause] /
#'   [`buildClauseGroup()`][picsure::buildClauseGroup]) to filter on, or `NULL`
#'   for an include-only query.
#' @param includeConcepts Optional character vector of *additional* concept
#'   paths to include as output columns, beyond the variables already named in
#'   `phenotypicFilter` (those are returned automatically). Order is preserved
#'   and duplicates are dropped.
#' @return An opaque Query handle.
#' @examples
#' \dontrun{
#' males <- picsure::buildClause("\\phs1\\sex\\", type = "FILTER", categories = "male")
#' q <- picsure::buildQuery(
#'   phenotypicFilter = males,
#'   includeConcepts = c("\\phs1\\bmi\\", "\\phs1\\hdl\\")
#' )
#' }
#' @export
buildQuery <- function(phenotypicFilter = NULL, includeConcepts = NULL) {
  if (!is.null(includeConcepts) &&
      (!is.character(includeConcepts) || any(is.na(includeConcepts)))) {
    stop("`includeConcepts` must be a character vector of concept paths, or NULL.")
  }

  kwargs <- drop_nulls(list(
    phenotypicFilter = phenotypicFilter,
    includeConcepts  = includeConcepts
  ))

  with_picsure_error(do.call(picsure_py$buildQuery, kwargs))
}
