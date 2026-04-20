# Public condition class for errors surfaced from the picsure package.
# Every Python PicSureError that bubbles up through a wrapper is caught and
# re-raised as a `picsureError` so users see a clean R-native error class
# rather than a python.builtin.Exception.

#' Construct a picsureError condition.
#'
#' @param message The user-facing error message. When the source is a Python
#'   PicSureError, use the Python-crafted message verbatim — Python already
#'   wrote it for researchers.
#' @param py_cause Optional; the original Python exception object. Stored on
#'   the condition as `$py_cause` for advanced debugging via
#'   `reticulate::py_last_error()`.
#' @return A condition of class `c("picsureError", "error", "condition")`.
#' @export
picsure_error <- function(message, py_cause = NULL) {
  structure(
    class = c("picsureError", "error", "condition"),
    list(message = message, py_cause = py_cause, call = sys.call(-1L))
  )
}
