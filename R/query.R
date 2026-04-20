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
#' @return Integer scalar for `"count"`; data.frame otherwise.
#' @examples
#' \dontrun{
#' count <- picsure::runQuery(bdc, full_query, type = "count")
#' rows  <- picsure::runQuery(bdc, full_query, type = "participant")
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
