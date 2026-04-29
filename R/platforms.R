#' List available PIC-SURE platforms.
#'
#' Returns the character vector of platform labels (or enum member names)
#' the Python `picsure` package recognizes. Pass one of these strings — or a
#' full URL to a custom PIC-SURE deployment — as the `platform` argument to
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
      vapply(py_members, function(m) m$value, character(1), USE.NAMES = FALSE)
    } else {
      unname(as.character(members))
    }
  })
}
