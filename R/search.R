#' Search the PIC-SURE data dictionary.
#'
#' Runs a keyword search against the session's dictionary resource and returns
#' the matching variables as a data frame.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param keyword A non-empty search string.
#' @param facets NULL (default) or a `FacetSet` from [`facets()`][picsure::facets].
#'   When supplied, the server narrows the search to variables inside the
#'   facets; Plan 2's Task 4 wires this argument in.
#' @param limit Optional integer. Maximum number of rows to return.
#' @param offset Optional integer. Row offset for pagination.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `Session.search()` call.
#' @return A `data.frame` of matching dictionary entries.
#' @examples
#' \dontrun{
#' bdc <- picsure::connect(platform = "BDC Authorized", token = my_token)
#' picsure::search(bdc, "sex")
#' }
#' @export
search <- function(session, keyword, facets = NULL, limit = NULL, offset = NULL, ...) {
  if (missing(keyword) || is.null(keyword) || is.na(keyword) || !nzchar(keyword)) {
    stop("`keyword` is required.")
  }

  kwargs <- drop_nulls(list(
    keyword = keyword,
    facets  = facets,
    limit   = as_py_int(limit),
    offset  = as_py_int(offset),
    ...
  ))

  with_picsure_error(do.call(session$search, kwargs))
}
