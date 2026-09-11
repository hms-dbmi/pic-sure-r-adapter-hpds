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
#'   `Session.searchDictionary()` call.
#' @return A `data.frame` of matching dictionary entries, with column types
#'   fixed by the dictionary schema whether or not the search matched
#'   anything: `conceptPath`, `name`, `display`, `description`, `dataType`,
#'   `studyId`, and `studyAcronym` are character, `min` and `max` numeric,
#'   `allowFiltering` logical, and `values` and `meta` list columns. A search
#'   that matched nothing returns a zero-row frame with those same types, so
#'   arithmetic on `min` / `max` behaves the same either way.
#' @examples
#' \dontrun{
#' bdc <- picsure::connect(platform = "BDC Authorized", token = my_token)
#' picsure::searchDictionary(bdc, "sex")
#' picsure::searchDictionary(bdc, "")  # all variables
#' }
#' @export
searchDictionary <- function(session, term = "", facets = NULL, include_values = TRUE, ...) {
  if (is.null(term) || length(term) != 1L || !is.character(term) || is.na(term)) {
    stop(.picsure_invalid_argument(sprintf(
      "`term` must be a single string (empty string is OK to fetch all); got %s.",
      describe_argument_value(term)
    )))
  }

  kwargs <- drop_nulls(list(
    term           = term,
    facets         = facets,
    include_values = include_values,
    ...
  ))

  apply_result_schema(
    with_picsure_error(do.call(session$searchDictionary, kwargs)),
    .DICTIONARY_RESULT_SCHEMA
  )
}

#' Look up valid values for a genomic annotation key (authorized platforms).
#'
#' Returns one page of values for a genomic key (e.g. all genes with variants),
#' as a data.frame with a single `value` column. Raise `size` to pull more
#' per call, or step `page` to walk the full set.
#'
#' @param session A session from [`connect()`][picsure::connect] on an
#'   authorized platform.
#' @param genomicConceptPath The genomic key, e.g. `"Gene_with_variant"` or
#'   `"Variant_consequence_calculated"`.
#' @param query Optional search term to narrow results (e.g. `"BRCA"`).
#' @param page 1-based page number. A single positive whole number; a
#'   fractional or non-numeric value raises a `picsureError` rather than
#'   being silently truncated or turned into `NA`.
#' @param size Page size (values per call). Same validation as `page`.
#' @param ... Additional keyword arguments forwarded to the Python call.
#' @return A data.frame with a single character `value` column. (The Python
#'   adapter's pagination metadata lives on the DataFrame's `.attrs` and does
#'   not survive reticulate conversion; paginate via `page`/`size`.)
#' @export
searchGenomicValues <- function(session, genomicConceptPath, query = "", page = 1, size = 100, ...) {
  if (missing(genomicConceptPath)) genomicConceptPath <- NULL
  as_single_string(
    genomicConceptPath, "genomicConceptPath",
    hint = "For example \"Gene_with_variant\"."
  )
  kwargs <- drop_nulls(list(
    genomicConceptPath = genomicConceptPath,
    query              = query,
    page               = as_positive_whole_number(page, "page"),
    size               = as_positive_whole_number(size, "size"),
    ...
  ))
  apply_result_schema(
    with_picsure_error(do.call(session$searchGenomicValues, kwargs)),
    .GENOMIC_VALUES_RESULT_SCHEMA
  )
}

#' Variant-consequence vocabulary (offline reference data).
#'
#' Returns the High/Medium/Low severity consequence vocabulary bundled with the
#' adapter as a data.frame (`severity`, `consequence`). No session or network
#' required.
#'
#' @return A data.frame with character columns `severity` and `consequence`.
#' @export
genomicConsequences <- function() {
  apply_result_schema(
    with_picsure_error(picsure_py$genomicConsequences()),
    .CONSEQUENCES_RESULT_SCHEMA
  )
}
