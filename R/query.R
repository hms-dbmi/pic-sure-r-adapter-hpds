#' Execute a query against a PIC-SURE session.
#'
#' Runs a query — a clause/clause-group from
#' [`buildClause()`][picsure::buildClause] /
#' [`buildClauseGroup()`][picsure::buildClauseGroup], or a Query from
#' [`buildQuery()`][picsure::buildQuery] — against the session's resource and
#' returns the result in the shape dictated by `type`:
#'
#' - `"count"` — a `CountResult` object with `$value` (exact count, or
#'   `NULL` for obfuscated small cohorts), `$margin`, and `$cap`.
#' - `"cross_count"` — a named list of `CountResult` objects keyed by
#'   concept path (reticulate converts the Python `dict`; the values stay
#'   Python objects).
#' - `"participant"` — data.frame with one row per matching participant
#'   across all included concepts.
#' - `"timestamp"` — data.frame of participant-level timestamps for
#'   longitudinal concepts.
#' - `"variant_count"` — a `CountResult` for the number of distinct matching
#'   variants (preserving obfuscation, like `"count"`).
#' - `"variant_list"` - a character vector of variant spec strings
#'   (not served by BDC primary environments yet).
#' - `"vcf_excerpt"` / `"aggregate_vcf_excerpt"` — a data.frame, one row per
#'   variant (the aggregate form omits per-patient columns)
#'   (not served by BDC primary environments yet).
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param query A clause/clause-group handle (from
#'   [`buildClause()`][picsure::buildClause] /
#'   [`buildClauseGroup()`][picsure::buildClauseGroup]) or a Query handle (from
#'   [`buildQuery()`][picsure::buildQuery]).
#' @param type A `QueryType` member (e.g.
#'   [`QueryType$COUNT`][picsure::QueryType]) or a case-insensitive
#'   string: `"count"` (default), `"participant"`, `"timestamp"`,
#'   `"cross_count"`, `"variant_count"`, `"variant_list"`, `"vcf_excerpt"`,
#'   or `"aggregate_vcf_excerpt"`.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `Session.runQuery()` call.
#' @return For `type = "count"` or `"variant_count"`, a Python `CountResult`
#'   object with `$value` (exact count, or `NULL` for obfuscated small
#'   cohorts), `$margin`, and `$cap`. For `type = "cross_count"`, a named
#'   list of `CountResult`s keyed by concept path. For `"participant"`,
#'   `"timestamp"`, `"vcf_excerpt"`, and `"aggregate_vcf_excerpt"`, a
#'   `data.frame`. For `"variant_list"`, a character vector.
#'
#'   A `"timestamp"` result is typed from the fixed timeseries schema HPDS
#'   declares — `PATIENT_NUM` integer, `CONCEPT_PATH` character, `NVAL_NUM`
#'   numeric, `TVAL_CHAR` character, `TIMESTAMP` character — rather than
#'   inferred from the rows. HPDS fills exactly one of `NVAL_NUM` /
#'   `TVAL_CHAR` per row, so a query over numeric concepts alone leaves
#'   `TVAL_CHAR` empty in every row, and inference used to hand that column
#'   back as numeric. `"participant"` and the VCF-excerpt results have a
#'   server-driven column set (one per concept), so their columns are left as
#'   they arrive.
#' @examples
#' \dontrun{
#' count <- picsure::runQuery(bdc, full_query, type = "count")
#' if (!is.null(count$value)) cat(count$value, "participants\n")
#'
#' rows <- picsure::runQuery(bdc, full_query, type = "participant")
#' }
#' @export
runQuery <- function(session, query, type = "count", ...) {
  if (missing(query) || is.null(query)) {
    stop(.picsure_invalid_argument(
      "`query` is required. Build one with picsure::buildClause(), picsure::buildClauseGroup(), or picsure::buildQuery."
    ))
  }

  kwargs <- drop_nulls(list(
    query = query,
    type  = to_py_enum(type, picsure_py$QueryType, "QueryType", "picsure_query_type"),
    ...
  ))

  apply_query_result_schema(
    with_picsure_error(do.call(session$runQuery, kwargs)),
    type
  )
}

# Types a query result from the schema its result type declares.
#
# Only the timeseries result has a fixed, server-declared column set (HPDS's
# TimeseriesProcessor always writes the same five columns). A participant or
# VCF-excerpt result's columns are the concepts the query asked for, so there
# is no schema to apply and the frame is returned as it arrived.
apply_query_result_schema <- function(result, type) {
  requested <- tryCatch(
    as_enum_string(type, "picsure_query_type", "QueryType", field = "name"),
    error = function(e) NULL
  )
  if (is.null(requested) || !identical(toupper(requested), "TIMESTAMP")) {
    return(result)
  }
  apply_result_schema(result, .TIMESERIES_RESULT_SCHEMA)
}

#' Load a previously-saved PIC-SURE query by its query ID.
#'
#' Fetches the saved query body from the PIC-SURE backend and rebuilds it
#' as a Clause/ClauseGroup (or a Query, when the saved query selected output
#' concepts) that can be passed back into [`runQuery()`][picsure::runQuery] or
#' [`exportAsPFB()`][picsure::exportAsPFB].
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param query_id The UUID string of a previously-saved query.
#' @return An opaque Clause / ClauseGroup handle suitable for `runQuery()`.
#' @examples
#' \dontrun{
#' previous <- picsure::loadQueryByID(bdc, "11111111-2222-3333-4444-555555555555")
#' count <- picsure::runQuery(bdc, previous, type = "count")
#' }
#' @export
loadQueryByID <- function(session, query_id) {
  if (missing(query_id)) query_id <- NULL
  as_single_string(query_id, "query_id", hint = "It is the saved query's UUID.")

  with_picsure_error(session$loadQueryByID(query_id))
}

#' Load a saved PIC-SURE query by ID and execute it in one call.
#'
#' Fetches a previously-saved query by UUID and runs it. Semantically
#' equivalent to [`loadQueryByID()`][picsure::loadQueryByID] followed by
#' [`runQuery()`][picsure::runQuery], but delegated to the Python adapter
#' as a single call. Returns the same result shapes as `runQuery()`.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param query_id The UUID string of a previously-saved query.
#' @param type A `QueryType` member (e.g.
#'   [`QueryType$COUNT`][picsure::QueryType]) or a case-insensitive
#'   string: `"count"` (default), `"participant"`, `"timestamp"`,
#'   `"cross_count"`, `"variant_count"`, `"variant_list"`, `"vcf_excerpt"`,
#'   or `"aggregate_vcf_excerpt"`.
#' @return Same as [`runQuery()`][picsure::runQuery]: a `CountResult` for
#'   `"count"` or `"variant_count"`, a named list of `CountResult`s keyed by
#'   concept path for `"cross_count"`,
#'   a `data.frame` for `"participant"` / `"timestamp"` / `"vcf_excerpt"` /
#'   `"aggregate_vcf_excerpt"` (not served by BDC primary environments yet),
#'   or a character vector for `"variant_list"` (not served by BDC primary
#'   environments yet).
#' @examples
#' \dontrun{
#' count <- picsure::runQueryByID(bdc, "11111111-2222-3333-4444-555555555555")
#' df    <- picsure::runQueryByID(bdc, "XXXXX-ID", type = "participant")
#' }
#' @export
runQueryByID <- function(session, query_id, type = "count") {
  if (missing(query_id)) query_id <- NULL
  as_single_string(query_id, "query_id", hint = "It is the saved query's UUID.")

  apply_query_result_schema(
    with_picsure_error(session$runQueryByID(
      query_id,
      type = to_py_enum(type, picsure_py$QueryType, "QueryType", "picsure_query_type")
    )),
    type
  )
}

#' Return a copy of a query with all matches of a sub-query removed.
#'
#' Matching is structural: any nested clause or clause-group that equals
#' `target` (by value) is dropped. Empty groups left behind are removed
#' automatically. Errors if the resulting query would be empty.
#'
#' @param query  A clause or clause-group handle (the query to edit).
#' @param target A clause or clause-group handle (the thing to remove).
#' @return A new clause or clause-group handle. The original `query` is
#'   not mutated.
#' @examples
#' \dontrun{
#' smaller <- picsure::removeSubQuery(full_query, age_filter)
#' }
#' @export
removeSubQuery <- function(query, target) {
  if (missing(query) || is.null(query)) {
    stop(.picsure_invalid_argument(
      "`query` is required. Pass the clause, clause-group, or query handle to edit."
    ))
  }
  if (missing(target) || is.null(target)) {
    stop(.picsure_invalid_argument(
      "`target` is required. Pass the clause or clause-group handle to remove."
    ))
  }
  with_picsure_error(picsure_py$removeSubQuery(query, target))
}

#' Return a copy of a query with one sub-query swapped for another.
#'
#' Matching is structural (see [`removeSubQuery()`][picsure::removeSubQuery]).
#'
#' @param query       A clause or clause-group handle (the query to edit).
#' @param target      A clause or clause-group handle (the thing to replace).
#' @param replacement A clause or clause-group handle (the substitute).
#' @return A new clause or clause-group handle.
#' @examples
#' \dontrun{
#' adjusted <- picsure::replaceClause(full_query, old_age, new_age)
#' }
#' @export
replaceClause <- function(query, target, replacement) {
  if (missing(query) || is.null(query) ||
      missing(target) || is.null(target) ||
      missing(replacement) || is.null(replacement)) {
    stop(.picsure_invalid_argument(
      "`query`, `target`, and `replacement` are all required."
    ))
  }
  with_picsure_error(picsure_py$replaceClause(query, target, replacement))
}

#' Save a query to the authenticated user's profile and return its query ID.
#'
#' Submits the query to PIC-SURE (creating a server-side query record), then
#' associates `name` with it via the `/dataset/named/` endpoint. The returned
#' UUID can later be passed to [`loadQueryByID()`][picsure::loadQueryByID]
#' or [`runQueryByID()`][picsure::runQueryByID]. Not supported on open-access
#' platforms.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param query   A clause or clause-group handle.
#' @param name    A non-empty character scalar. Allowed characters: letters,
#'   digits, spaces, and `- _ \\ / ? + = [ ] . ( ) : " '`. Max 255 chars.
#' @param overwrite Logical. When `FALSE` (default), errors if a named
#'   query with `name` already exists for this user. When `TRUE`, the
#'   existing record is updated to point at the freshly-submitted query.
#' @return A character scalar -- the PIC-SURE query ID.
#' @examples
#' \dontrun{
#' qid <- picsure::saveQueryByName(bdc, my_query, "Cohort 2026-Q2")
#' qid <- picsure::saveQueryByName(bdc, my_query, "Cohort 2026-Q2", overwrite = TRUE)
#' later <- picsure::loadQueryByID(bdc, qid)
#' }
#' @export
saveQueryByName <- function(session, query, name, overwrite = FALSE) {
  if (missing(query) || is.null(query)) {
    stop(.picsure_invalid_argument(
      "`query` is required. Build one with picsure::buildClause(), picsure::buildClauseGroup(), or picsure::buildQuery."
    ))
  }
  if (missing(name)) name <- NULL
  as_single_string(name, "name")
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop(.picsure_invalid_argument(sprintf(
      "`overwrite` must be a single logical (TRUE or FALSE); got %s.",
      describe_argument_value(overwrite)
    )))
  }
  with_picsure_error(session$saveQueryByName(query, name, overwrite = overwrite))
}
