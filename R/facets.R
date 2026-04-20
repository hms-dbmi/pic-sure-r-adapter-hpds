#' Build a FacetSet for narrowing search results.
#'
#' Returns an opaque `FacetSet` handle tied to this session. Add entries with
#' [`addFacet()`][picsure::addFacet] and remove them with
#' [`removeFacet()`][picsure::removeFacet]. Pass the final FacetSet to
#' [`search()`][picsure::search] via the `facets` argument.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @return An opaque FacetSet handle.
#' @examples
#' \dontrun{
#' fs <- picsure::facets(bdc)
#' fs <- picsure::addFacet(fs, "study_ids", "phs000007")
#' picsure::search(bdc, "sex", facets = fs)
#' }
#' @export
facets <- function(session) {
  with_picsure_error(session$facets())
}
