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
      stop(picsureError(
        "picsure_py$Platform is NULL; reticulate bindings may not be initialized.",
        class = "picsureConnectionError"
      ))
    }
    .platform_labels(.platform_members(members))
  })
}

#' Convert a Python `Platform` enum class into a plain R list of its members.
#'
#' `Platform.__members__` is a `mappingproxy`, which reticulate cannot
#' convert. `py_to_r()` hands the proxy back unchanged, and iterating it from
#' R fails with "cannot coerce type 'environment' to vector of type 'list'".
#' Copying the proxy into a real `dict` first gives reticulate a type it does
#' convert.
#'
#' @param platform_enum The Python enum class. An already-converted named
#'   list or a character vector of labels is returned untouched.
#' @return A named list of members, or the non-Python input unchanged.
#' @noRd
.platform_members <- function(platform_enum) {
  if (!inherits(platform_enum, "python.builtin.object")) {
    return(platform_enum)
  }
  mapping <- platform_enum$`__members__`
  if (inherits(mapping, "python.builtin.object")) {
    builtins <- reticulate::import_builtins()
    mapping <- reticulate::py_to_r(builtins$dict(mapping))
  }
  as.list(mapping)
}

#' Read the display label of each member of a converted Platform enum.
#'
#' @param members The named list `.platform_members()` produces, or a plain
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
