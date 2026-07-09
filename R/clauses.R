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

#' Create a single genomic (variant) filter.
#'
#' Builds an opaque GenomicFilter handle for the `genomicFilters` argument of
#' [`buildQuery()`][picsure::buildQuery]. A filter is **categorical**: it
#' matches when the annotation named by `key` is one of `values`.
#'
#' @param key The genomic annotation to filter on: a
#'   [`GenomicFilterKey`][picsure::GenomicFilterKey] member (preferred) or the
#'   equivalent string (validated Python-side). `GenomicFilterKey$VARIANT_SEVERITY`
#'   is a virtual key — the Python adapter expands the requested
#'   [`VariantSeverity`][picsure::VariantSeverity] buckets into
#'   `Variant_consequence_calculated` values. Variant-spec (SNP) keys are
#'   rejected.
#' @param values Required. Categorical value(s): a character vector, or
#'   [`VariantFrequency`][picsure::VariantFrequency] members (coerced to their
#'   string value).
#' @param ... Additional keyword arguments forwarded to the Python
#'   `picsure.buildGenomicFilter()` call.
#' @return An opaque GenomicFilter handle.
#' @examples
#' \dontrun{
#' gene <- picsure::buildGenomicFilter("Gene_with_variant", values = c("BRCA1"))
#' rare <- picsure::buildGenomicFilter(
#'   "Variant_frequency_as_text", values = picsure::VariantFrequency$RARE
#' )
#' }
#' @export
buildGenomicFilter <- function(key, values = NULL, ...) {
  if (!missing(key) && inherits(key, "picsure_enum_member")) {
    if (!inherits(key, "picsure_genomic_filter_key")) {
      stop("`key` must be a GenomicFilterKey member or a non-empty character scalar.")
    }
    key <- key$value
  } else if (missing(key) || is.null(key) || !is.character(key) ||
             length(key) != 1L || is.na(key) || !nzchar(key)) {
    stop("`key` must be a GenomicFilterKey member or a non-empty character scalar (e.g. \"Gene_with_variant\").")
  }
  if (!is.null(values)) {
    if (inherits(values, "picsure_enum_member")) {
      values <- list(values)
    }
    values <- vapply(values, function(v) {
      if (inherits(v, "picsure_enum_member")) v$value else as.character(v)
    }, character(1), USE.NAMES = FALSE)
  }

  kwargs <- drop_nulls(list(
    key    = key,
    values = values,
    ...
  ))

  with_picsure_error(do.call(picsure_py$buildGenomicFilter, kwargs))
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
#' @param genomicFilters A GenomicFilter handle (from
#'   [`buildGenomicFilter()`][picsure::buildGenomicFilter]) or a list of them,
#'   applied as a flat conjunctive list. `NULL` (default) for no genomic filter.
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
buildQuery <- function(phenotypicFilter = NULL, includeConcepts = NULL, genomicFilters = NULL) {
  if (!is.null(includeConcepts) &&
      (!is.character(includeConcepts) || any(is.na(includeConcepts)))) {
    stop("`includeConcepts` must be a character vector of concept paths, or NULL.")
  }

  kwargs <- drop_nulls(list(
    phenotypicFilter = phenotypicFilter,
    includeConcepts  = includeConcepts,
    genomicFilters   = genomicFilters
  ))

  with_picsure_error(do.call(picsure_py$buildQuery, kwargs))
}
