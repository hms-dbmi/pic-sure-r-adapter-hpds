#' Export query results to a PFB file.
#'
#' Runs the query and writes the result as a PFB (Portable Format for
#' Bioinformatics) file at `path`. Requires the Python `picsure[pfb]` optional
#' dependency on the Python side.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param query A clause group from
#'   [`buildClauseGroup()`][picsure::buildClauseGroup].
#' @param path Destination file path.
#' @return The path, invisibly.
#' @examples
#' \dontrun{
#' picsure::exportAsPFB(bdc, full_query, "~/cohort.pfb")
#' }
#' @export
exportAsPFB <- function(session, query, path) {
  if (missing(query) || is.null(query)) {
    stop("`query` is required. Build one with picsure::buildClauseGroup().")
  }
  if (missing(path) || is.null(path) || is.na(path) || !nzchar(path)) {
    stop("`path` is required. Provide a writable file path as a string.")
  }

  with_picsure_error(session$exportAsPFB(query, path))
  invisible(path)
}

#' Write a participant data frame to a CSV file.
#'
#' Unlike [`exportAsPFB()`][picsure::exportAsPFB], `exportCSV` and `exportTSV`
#' write an already-materialized data frame — they do not re-run a query.
#' The typical flow is `runQuery(..., type = "participant")` followed by
#' `exportCSV(session, df, path)`.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param data A `data.frame` (typically from
#'   [`runQuery()`][picsure::runQuery] with `type = "participant"` or
#'   `"timestamp"`).
#' @param path Destination file path.
#' @return The path, invisibly.
#' @examples
#' \dontrun{
#' df <- picsure::runQuery(bdc, full_query, type = "participant")
#' picsure::exportCSV(bdc, df, "~/cohort.csv")
#' }
#' @export
exportCSV <- function(session, data, path) {
  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    stop("`data` must be a data.frame. Run picsure::runQuery(session, query, type = \"participant\") first and pass its result.")
  }
  if (missing(path) || is.null(path) || is.na(path) || !nzchar(path)) {
    stop("`path` is required. Provide a writable file path as a string.")
  }

  with_picsure_error(session$exportCSV(data, path))
  invisible(path)
}

#' Write a participant data frame to a TSV file.
#'
#' See [`exportCSV()`][picsure::exportCSV]; semantics are identical, output is
#' tab-separated.
#'
#' @inheritParams exportCSV
#' @return The path, invisibly.
#' @examples
#' \dontrun{
#' df <- picsure::runQuery(bdc, full_query, type = "participant")
#' picsure::exportTSV(bdc, df, "~/cohort.tsv")
#' }
#' @export
exportTSV <- function(session, data, path) {
  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    stop("`data` must be a data.frame. Run picsure::runQuery(session, query, type = \"participant\") first and pass its result.")
  }
  if (missing(path) || is.null(path) || is.na(path) || !nzchar(path)) {
    stop("`path` is required. Provide a writable file path as a string.")
  }

  with_picsure_error(session$exportTSV(data, path))
  invisible(path)
}
