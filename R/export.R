#' Export query results to a PFB file.
#'
#' Runs the query, materializes the result as a PFB (Portable Format for
#' Bioinformatics) file at `path`, and returns the path invisibly. Requires
#' the Python `picsure[pfb]` optional dependency on the Python side.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param query A clause group from
#'   [`buildClauseGroup()`][picsure::buildClauseGroup].
#' @param path Destination file path.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `Session.exportPFB()` call.
#' @return The path, invisibly.
#' @examples
#' \dontrun{
#' picsure::exportPFB(bdc, full_query, "~/cohort.pfb")
#' }
#' @export
exportPFB <- function(session, query, path, ...) {
  if (missing(query) || is.null(query)) {
    stop("`query` is required.")
  }
  if (missing(path) || is.null(path) || is.na(path) || !nzchar(path)) {
    stop("`path` is required.")
  }

  kwargs <- drop_nulls(list(
    query = query,
    path  = path,
    ...
  ))

  with_picsure_error(do.call(session$exportPFB, kwargs))
  invisible(path)
}
