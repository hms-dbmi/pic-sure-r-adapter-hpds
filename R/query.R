#' Execute a query against a PIC-SURE session.
#'
#' Runs the query tree produced by [`buildClauseGroup()`][picsure::buildClauseGroup]
#' against the session's resource and returns the result in the shape
#' dictated by `type`:
#'
#' - `"count"` — integer scalar, the number of matching participants.
#' - `"participant"` — data.frame with one row per matching participant
#'   across all SELECTed variables.
#' - `"timestamp"` — data.frame of participant-level timestamps for
#'   longitudinal concepts.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param query A clause group from
#'   [`buildClauseGroup()`][picsure::buildClauseGroup].
#' @param type A case-insensitive string: `"count"` (default),
#'   `"participant"`, or `"timestamp"`.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `Session.runQuery()` call.
#' @return For `type = "count"`, a Python `CountResult` object with
#'   `$value` (exact count, or `NULL` for obfuscated small cohorts),
#'   `$margin`, and `$cap`. For `type = "cross_count"`, a dict-like
#'   mapping concept paths to CountResults. For `"participant"` and
#'   `"timestamp"`, a `data.frame`.
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
    stop("`query` is required. Build one with picsure::buildClauseGroup().")
  }

  kwargs <- drop_nulls(list(
    query = query,
    type  = type,
    ...
  ))

  with_picsure_error(do.call(session$runQuery, kwargs))
}

#' Load a previously-saved PIC-SURE query by its query ID.
#'
#' Fetches the saved query body from the PIC-SURE backend and rebuilds it
#' as a Clause or ClauseGroup that can be passed back into
#' [`runQuery()`][picsure::runQuery], [`exportPFB()`][picsure::exportPFB],
#' or composed inside another [`buildClauseGroup()`][picsure::buildClauseGroup].
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
  if (missing(query_id) || is.null(query_id) ||
      !is.character(query_id) || length(query_id) != 1L ||
      is.na(query_id) || !nzchar(query_id)) {
    stop("`query_id` must be a non-empty character string (the saved query's UUID).")
  }

  with_picsure_error(session$loadQueryByID(query_id))
}
