#' Connect to a PIC-SURE instance.
#'
#' Opens a session against the named PIC-SURE platform using the supplied
#' personal token. Platforms are listed by `picsure::platforms()`.
#'
#' On the first call of the R session, reticulate provisions an isolated
#' Python environment containing the `picsure` Python package. This may take
#' a few seconds the first time; subsequent calls reuse the cached env.
#'
#' @param platform A platform name, e.g. `"Demo"`, `"BDC Open"`, or
#'   `"BDC Authorized"`.
#' @param token Your personal PIC-SURE access token, obtained from the
#'   "User Profile" tab of your PIC-SURE instance.
#' @param ... Additional keyword arguments forwarded to the Python
#'   `picsure.connect()` call.
#' @return An opaque session object. Pass it as the first argument to
#'   `picsure::search()`, `picsure::runQuery()`, and friends.
#' @examples
#' \dontrun{
#' bdc <- picsure::connect(platform = "BDC Authorized", token = my_token)
#' }
#' @export
connect <- function(platform, token, ...) {
  bad_platform_msg <- "`platform` is required. Call picsure::platforms() to list valid values."
  if (missing(platform) || is.null(platform)) {
    stop(bad_platform_msg)
  }
  if (is.character(platform)) {
    if (length(platform) != 1L || is.na(platform) || !nzchar(platform)) {
      stop(bad_platform_msg)
    }
  } else if (!inherits(platform, "python.builtin.object")) {
    # Reject logicals like NA, numerics, lists, etc.; accept only strings or
    # Python objects (e.g. a reticulate-wrapped Platform enum member).
    stop(bad_platform_msg)
  }
  if (missing(token) || is.null(token) || is.na(token) || !nzchar(token)) {
    stop("`token` is required. Copy it from the 'User Profile' tab of PIC-SURE.")
  }

  kwargs <- drop_nulls(list(
    platform = platform,
    token    = token,
    ...
  ))

  with_picsure_error(do.call(picsure_py$connect, kwargs))
}
