#' List available PIC-SURE platforms.
#'
#' Returns the character vector of human-readable platform labels the
#' Python `picsure` package recognizes. These labels are display-only:
#' [`connect()`][picsure::connect] does **not** accept them. To connect,
#' pass a [`Platform`][picsure::Platform] member (e.g.
#' `Platform$BDC_AUTHORIZED`) or a full URL to a custom PIC-SURE
#' deployment.
#'
#' @return A character vector.
#' @examples
#' \dontrun{
#' picsure::platforms()
#' }
#' @seealso [`Platform`][picsure::Platform], the R-side enum of known
#'   platforms with attached connection details.
#' @export
platforms <- function() {
  with_picsure_error({
    members <- picsure_py$Platform
    if (is.null(members)) {
      .picsure_reject(
        "picsure_py$Platform is NULL; reticulate bindings may not be initialized.",
        class = NULL
      )
    }
    .platform_labels(.py_enum_members(members))
  })
}

#' Read the display label of each member of a converted Platform enum.
#'
#' @param members The named list `.py_enum_members()` produces, or a plain
#'   character vector of labels.
#' @return An unnamed character vector of labels.
#' @noRd
.platform_labels <- function(members) {
  if (is.character(members)) {
    return(unname(members))
  }
  vapply(as.list(members), .platform_label, character(1), USE.NAMES = FALSE)
}

#' Read one Platform member's label.
#'
#' A member's `value` is the `PlatformConfig` dataclass, not a string, so the
#' label lives at `value$label`. Reading `value` directly returns the
#' dataclass and breaks `vapply(..., character(1))`.
#'
#' @param member One converted enum member, or a bare label string.
#' @return The label as a character scalar.
#' @noRd
.platform_label <- function(member) {
  if (is.character(member)) {
    return(member)
  }
  member$value$label
}
