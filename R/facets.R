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

#' Add an entry to a FacetSet.
#'
#' Mutates the underlying Python FacetSet and returns the same handle for
#' chaining. If `value` is a vector of length > 1, adds one entry per value
#' (all under the same `key`).
#'
#' @param facet_set A FacetSet from [`facets()`][picsure::facets].
#' @param key Facet key, e.g. `"study_ids"`.
#' @param value Facet value (scalar or vector).
#' @return The same FacetSet, invisibly, for chaining.
#' @examples
#' \dontrun{
#' fs <- picsure::facets(bdc)
#' fs <- picsure::addFacet(fs, "study_ids", "phs000007")
#' fs <- picsure::addFacet(fs, "study_ids", c("phs000200", "phs000286"))
#' }
#' @export
addFacet <- function(facet_set, key, value) {
  if (missing(key) || is.null(key) || is.na(key) || !nzchar(key)) {
    stop("`key` is required. Pick a facet category, e.g. \"study_ids\" or \"data_source\".")
  }
  if (missing(value) || is.null(value)) {
    stop("`value` is required. Pass the facet value (or vector of values) to add.")
  }
  for (v in value) {
    with_picsure_error(facet_set$add(key, v))
  }
  invisible(facet_set)
}

#' Remove an entry from a FacetSet.
#'
#' Mutates the underlying Python FacetSet and returns the same handle for
#' chaining.
#'
#' @param facet_set A FacetSet from [`facets()`][picsure::facets].
#' @param key Facet key.
#' @param value Facet value (scalar only).
#' @return The same FacetSet, invisibly, for chaining.
#' @examples
#' \dontrun{
#' fs <- picsure::facets(bdc)
#' fs <- picsure::addFacet(fs, "study_ids", "phs000007")
#' fs <- picsure::removeFacet(fs, "study_ids", "phs000007")
#' }
#' @export
removeFacet <- function(facet_set, key, value) {
  if (missing(key) || is.null(key) || is.na(key) || !nzchar(key)) {
    stop("`key` is required. Pick a facet category, e.g. \"study_ids\" or \"data_source\".")
  }
  if (missing(value) || is.null(value)) {
    stop("`value` is required. Pass the facet value to remove.")
  }
  with_picsure_error(facet_set$remove(key, value))
  invisible(facet_set)
}
