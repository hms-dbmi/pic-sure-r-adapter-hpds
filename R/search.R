#' Search the PIC-SURE data dictionary.
#'
#' Runs a keyword search against the session's dictionary resource and returns
#' the matching variables as a data frame. An empty `term` returns every
#' variable the session has access to.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @param term Search string; empty string returns all variables.
#' @param facets NULL (default) or a `FacetSet` from [`facets()`][picsure::facets].
#'   When supplied, the server narrows results to variables inside the facets.
#' @param include_values If TRUE (default), variable values are included in
#'   the response. Set FALSE to omit them for a lighter payload.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `Session.search()` call.
#' @return A `data.frame` of matching dictionary entries.
#' @examples
#' \dontrun{
#' bdc <- picsure::connect(platform = "BDC Authorized", token = my_token)
#' picsure::dictionarySearch(bdc, "sex")
#' picsure::dictionarySearch(bdc, "")  # all variables
#' }
#' @export
dictionarySearch <- function(session, term = "", facets = NULL, include_values = TRUE, ...) {
  if (is.null(term) || length(term) != 1L || is.na(term) || !is.character(term)) {
    stop("`term` must be a single string (empty string is OK to fetch all).")
  }

  kwargs <- drop_nulls(list(
    term           = term,
    facets         = facets,
    include_values = include_values,
    ...
  ))

  with_picsure_error(do.call(session$search, kwargs))
}
