#' Build a FacetSet for narrowing search results.
#'
#' Returns an opaque `FacetSet` handle tied to this session. Add entries with
#' [`addFacet()`][picsure::addFacet] and remove them with
#' [`removeFacet()`][picsure::removeFacet]. Pass the final FacetSet to
#' [`searchDictionary()`][picsure::searchDictionary] via the `facets` argument.
#'
#' The handle is created from the categories the *deployment* publishes, so
#' the valid category names come from the server rather than from this
#' package. The dictionary ETL creates two on every deployment:
#' `"dataset_id"`, the study or dataset (e.g. a dbGaP accession), and
#' `"data_type"`, `"categorical"` or `"continuous"`. Deployments with
#' genomic data also get `"data_source"`. A category the server did not
#' publish raises a `picsureError` listing the ones it did.
#'
#' @param session A session object produced by [`connect()`][picsure::connect].
#' @return An opaque FacetSet handle.
#' @examples
#' \dontrun{
#' fs <- picsure::facets(bdc)
#' fs <- picsure::addFacet(fs, "dataset_id", "phs000007")
#' picsure::searchDictionary(bdc, "sex", facets = fs)
#' }
#' @export
facets <- function(session) {
  with_picsure_error(session$facets())
}

#' Reject a facet key that is not one non-empty string.
#'
#' A facet entry selects values inside one category. The Python FacetSet's
#' `add(category, values)` takes a single category and a vector of values, so
#' a vector of values is fine and a vector of keys is not. Checking length
#' first keeps a length-2 key out of `is.na(key)` inside an `||`, which base R
#' rejects with a condition-length error that does not name the argument.
#'
#' @param key The value passed as `key`.
#' @param call The call to report in the error, by default the caller's.
#' @return `key` unchanged.
#' @noRd
.check_facet_key <- function(key, call = sys.call(-1L)) {
  if (is.null(key) || length(key) != 1L || !is.character(key) ||
      is.na(key) || !nzchar(key)) {
    .picsure_reject(
      sprintf(
        paste0(
          "`key` must be a single facet category name; got %s. A facet entry ",
          "selects values inside one category, so pass one key (e.g. ",
          "\"dataset_id\" or \"data_type\") and give the vector to `value`."
        ),
        describe_argument_value(key)
      ),
      call = call
    )
  }
  key
}

#' Add an entry to a FacetSet.
#'
#' Mutates the underlying Python FacetSet and returns the same handle for
#' chaining. If `value` is a vector of length > 1, adds one entry per value
#' (all under the same `key`).
#'
#' @param facet_set A FacetSet from [`facets()`][picsure::facets].
#' @param key Facet category name, e.g. `"dataset_id"` or `"data_type"`.
#'   Exactly one category. A facet entry selects values inside a single
#'   category, so a vector of keys raises a `picsureError`. The valid
#'   category names come from the server, not from this package; a name the
#'   deployment does not publish raises a `picsureError` listing the ones it
#'   does.
#' @param value Facet value, or a vector of values under the same `key`.
#' @return The same FacetSet, invisibly, for chaining.
#' @examples
#' \dontrun{
#' fs <- picsure::facets(bdc)
#' fs <- picsure::addFacet(fs, "dataset_id", "phs000007")
#' fs <- picsure::addFacet(fs, "dataset_id", c("phs000200", "phs000286"))
#' }
#' @export
addFacet <- function(facet_set, key, value) {
  if (missing(key)) key <- NULL
  .check_facet_key(key)
  if (missing(value) || is.null(value) || length(value) == 0L) {
    .picsure_reject(
      "`value` is required. Pass the facet value (or vector of values) to add."
    )
  }
  for (v in value) {
    with_picsure_error(facet_set$add(key, v))
  }
  invisible(facet_set)
}

#' Compute what a facet category holds once some values are removed.
#'
#' The Python FacetSet exposes `add()`, `view()`, and `clear()` but no
#' per-value removal, so [`removeFacet()`][picsure::removeFacet] reads the
#' current selections, drops the ones being removed, and rewrites the
#' category. `view()` reports every available category, so a key with no
#' selections and an unknown key both yield an empty vector.
#'
#' @param view The named list returned by the Python FacetSet's `view()`.
#' @param key Facet category name.
#' @param value Value, or vector of values, being removed.
#' @return A character vector of the values to keep, in their original order.
#' @keywords internal
facet_values_after_removal <- function(view, key, value) {
  current <- as.character(unlist(view[[key]], use.names = FALSE))
  current[!current %in% as.character(value)]
}

#' Remove an entry from a FacetSet.
#'
#' Mutates the underlying Python FacetSet and returns the same handle for
#' chaining. Every entry under `key` matching `value` is dropped; removing a
#' value that was never added leaves the FacetSet unchanged.
#'
#' The Python FacetSet has no per-value removal, so the category is rebuilt:
#' the survivors are computed first, the category is cleared, and the
#' survivors are added back. If adding them back fails, the original
#' selection is restored before the error is raised, so a failed removal
#' never leaves the category empty.
#'
#' @param facet_set A FacetSet from [`facets()`][picsure::facets].
#' @param key Facet key. Exactly one category, as for
#'   [`addFacet()`][picsure::addFacet].
#' @param value Facet value, or a vector of values under the same `key`.
#' @return The same FacetSet, invisibly, for chaining.
#' @examples
#' \dontrun{
#' fs <- picsure::facets(bdc)
#' fs <- picsure::addFacet(fs, "dataset_id", "phs000007")
#' fs <- picsure::removeFacet(fs, "dataset_id", "phs000007")
#' }
#' @export
removeFacet <- function(facet_set, key, value) {
  if (missing(key)) key <- NULL
  .check_facet_key(key)
  if (missing(value) || is.null(value) || length(value) == 0L) {
    .picsure_reject(
      "`value` is required. Pass the facet value (or vector of values) to remove."
    )
  }
  view <- with_picsure_error(facet_set$view())
  original <- as.character(unlist(view[[key]], use.names = FALSE))
  retained <- facet_values_after_removal(view, key, value)
  with_picsure_error(facet_set$clear(key))
  if (length(retained) > 0L) {
    tryCatch(
      with_picsure_error(facet_set$add(key, as.list(retained))),
      error = function(e) {
        try(facet_set$add(key, as.list(original)), silent = TRUE)
        stop(e)
      }
    )
  }
  invisible(facet_set)
}
