#' List available PIC-SURE platforms.
#'
#' Returns the character vector of platform labels the Python `picsure`
#' package recognizes. Pass one of these strings — or a full URL to a custom
#' PIC-SURE deployment — as the `platform` argument to
#' [`connect()`][picsure::connect].
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
      stop("picsure_py$Platform is NULL; reticulate bindings may not be initialized.")
    }
    if (inherits(members, "python.builtin.object")) {
      py_members <- reticulate::py_to_r(members$`__members__`)
      .platform_labels(py_members)
    } else {
      unname(as.character(members))
    }
  })
}

# Extracts the `value$label` field from each member of a Platform enum's
# `__members__` map (after reticulate has converted it to an R list).
# Split out so unit tests can exercise the field reads without standing up a
# real Python session — the bug it guards against was reading `m$value`
# directly, which on a real Python enum returns a `PlatformConfig` dataclass
# rather than a string and would crash `vapply(..., character(1))`.
.platform_labels <- function(py_members) {
  vapply(py_members, function(m) m$value$label, character(1), USE.NAMES = FALSE)
}
