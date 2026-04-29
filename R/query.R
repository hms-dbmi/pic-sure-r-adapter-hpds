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
#' @param type A `QueryType` member (e.g.
#'   [`QueryType$COUNT`][picsure::QueryType]) or a case-insensitive
#'   string: `"count"` (default), `"participant"`, `"timestamp"`, or
#'   `"cross_count"`.
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
    type  = to_py_enum(type, picsure_py$QueryType, "QueryType", "picsure_query_type"),
    ...
  ))

  with_picsure_error(do.call(session$runQuery, kwargs))
}
