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

# Converts a Python `Platform` enum class into a plain R list of its members.
#
# `Platform.__members__` is a `mappingproxy`, and reticulate has no converter
# for that type: `py_to_r()` hands the proxy straight back, still classed
# `python.builtin.mappingproxy`, and iterating it from R then dies with
# "cannot coerce type 'environment' to vector of type 'list'". Copying the
# proxy into a real `dict` first gives reticulate something it knows how to
# convert. Input that is not a Python object — an already-converted list, or a
# character vector of labels — is returned untouched.
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

# Reads the display label out of each member of a converted Platform enum.
#
# Accepts either the named list `.platform_members()` produces or a plain
# character vector of labels, so the result does not depend on which shape
# reaches it.
.platform_labels <- function(members) {
  if (is.character(members)) {
    return(unname(members))
  }
  vapply(as.list(members), .platform_label, character(1), USE.NAMES = FALSE)
}

# Reads one member's label. A member's `value` is the `PlatformConfig`
# dataclass, not a string, so the label lives at `value$label`; reading
# `value` directly returns the dataclass and breaks `vapply(..., character(1))`.
.platform_label <- function(member) {
  if (is.character(member)) {
    return(member)
  }
  member$value$label
}
