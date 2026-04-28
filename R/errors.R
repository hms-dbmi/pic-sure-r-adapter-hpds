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
#' @examples
#' \dontrun{
#' tryCatch(
#'   picsure::connect(platform = "BDC Authorized", token = "bad-token"),
#'   picsureError = function(e) {
#'     message("PIC-SURE error: ", conditionMessage(e))
#'   }
#' )
#' }
#' @export
picsureError <- function(message, py_cause = NULL) {
  structure(
    class = c("picsureError", "error", "condition"),
    list(message = message, py_cause = py_cause, call = sys.call(-1L))
  )
}

#' Wrap a Python call so Python exceptions surface as picsureErrors.
#'
#' Any condition of class `python.builtin.Exception` thrown inside `expr` is
#' caught and re-raised as a `picsureError`. The message comes from the Python
#' exception (which the Python package already crafted for researchers), and
#' the original exception is attached as `$py_cause`. Non-Python R errors pass
#' through unchanged.
#'
#' @param expr An expression, typically a reticulate method call.
#' @return The value of `expr` if no error occurred.
#' @keywords internal
with_picsure_error <- function(expr) {
  tryCatch(
    expr,
    python.builtin.Exception = function(e) {
      stop(picsureError(conditionMessage(e), py_cause = e))
    }
  )
}
