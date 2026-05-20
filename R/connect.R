#' Connect to a PIC-SURE instance.
#'
#' Opens a session against the named PIC-SURE platform using the supplied
#' personal token. Platforms are listed by `picsure::platforms()`.
#'
#' On the first call of the R session, reticulate provisions an isolated
#' Python environment containing the `picsure` Python package. This may take
#' a few seconds the first time; subsequent calls reuse the cached env.
#'
#' @param platform A `Platform` member (e.g.
#'   [`Platform$BDC_OPEN`][picsure::Platform]) or a platform-label string
#'   (e.g. `"BDC Open"`, `"BDC Authorized"`).
#' @param token Your personal PIC-SURE access token, obtained from the
#'   "User Profile" tab of your PIC-SURE instance. Optional for open-access
#'   platforms (`Platform$BDC_OPEN`, `Platform$BDC_DEV_OPEN`,
#'   `Platform$BDC_PREDEV_OPEN`, `Platform$NHANES_OPEN`); required for
#'   authenticated platforms. Defaults to `""`.
#' @param ... Optional keyword arguments forwarded to the Python
#'   `picsure.connect()` call. Unknown keys raise a `picsureError` with the
#'   list of valid keys. Supported keys:
#'   \describe{
#'     \item{`resource_uuid`}{UUID of a specific PIC-SURE resource to connect
#'       to; overrides the platform default.}
#'     \item{`include_consents`}{Logical. When `TRUE`, the session retrieves
#'       the user's consent metadata from the auth service. Defaults to the
#'       Python adapter's choice (currently `TRUE`).}
#'     \item{`requires_auth`}{Logical. When `FALSE`, the session is opened in
#'       unauthenticated mode (only useful for open resources). Defaults to
#'       the Python adapter's choice (currently `TRUE`).}
#'   }
#' @return An opaque session object. Pass it as the first argument to
#'   `picsure::searchDictionary()`, `picsure::runQuery()`, and friends.
#' @examples
#' \dontrun{
#' bdc <- picsure::connect(platform = "BDC Authorized", token = my_token)
#' open <- picsure::connect(platform = picsure::Platform$BDC_OPEN)
#' }
#' @export
connect <- function(platform, token = "", ...) {
  bad_platform_msg <- "`platform` is required. Call picsure::platforms() to list valid values."
  if (missing(platform) || is.null(platform)) {
    stop(picsureError(bad_platform_msg))
  }
  # Track whether the platform is identifiably auth-required from R-side
  # info alone. For label strings or raw Python objects we can't tell
  # here and defer to Python's resolve_platform check.
  requires_auth_known <- inherits(platform, "picsure_platform") &&
                         isTRUE(platform$requires_auth)
  if (inherits(platform, "picsure_enum_member")) {
    if (!inherits(platform, "picsure_platform")) {
      stop(picsureError(sprintf(
        "Expected a Platform member, got %s.", format(platform)
      )))
    }
    # Convert the R-side member to the Python Platform enum member by
    # name. Going through the proxy avoids the URL/label ambiguity:
    # BDC_AUTHORIZED and BDC_OPEN share a URL (distinguished only by
    # resource_uuid); BDC_AUTHORIZED and BDC_DEV_AUTHORIZED share a
    # label.
    platform <- picsure_py$Platform[[platform$name]]
  } else if (is.character(platform)) {
    if (length(platform) != 1L || is.na(platform) || !nzchar(platform)) {
      stop(picsureError(bad_platform_msg))
    }
  } else if (!inherits(platform, "python.builtin.object")) {
    # Reject logicals like NA, numerics, lists, etc.; accept only strings,
    # picsure_platform members, or Python objects (e.g. a reticulate-
    # wrapped Platform enum member).
    stop(picsureError(bad_platform_msg))
  }
  # Token type validation runs regardless of platform. The non-empty
  # check fires only when the R-side platform info confirms auth is
  # required; otherwise Python's own check (which sees the resolved
  # PlatformInfo) decides.
  if (is.null(token)) token <- ""
  if (!is.character(token) || length(token) != 1L || is.na(token)) {
    stop(picsureError(
      "`token` must be a single character string (use \"\" for open platforms)."
    ))
  }
  if (requires_auth_known && !nzchar(token)) {
    stop(picsureError(
      "`token` is required for this platform. Copy it from the 'User Profile' tab of PIC-SURE."
    ))
  }

  extras <- list(...)
  unknown <- setdiff(names(extras), CONNECT_EXTRA_KWARGS)
  if (length(unknown) > 0) {
    stop(picsureError(sprintf(
      "Unknown argument(s) to connect(): %s. Valid extras: %s.",
      paste(sprintf("`%s`", unknown), collapse = ", "),
      paste(sprintf("`%s`", CONNECT_EXTRA_KWARGS), collapse = ", ")
    )))
  }

  kwargs <- drop_nulls(c(
    list(platform = platform, token = token),
    extras
  ))

  with_picsure_error(do.call(picsure_py$connect, kwargs))
}

# Whitelist of optional kwargs forwarded through `...` to picsure_py$connect.
# Names mirror the Python adapter's snake_case kwargs 1:1 — no R-side
# translation. To bump: add the new kwarg here and to connect()'s @param block.
CONNECT_EXTRA_KWARGS <- c("resource_uuid", "include_consents", "requires_auth")
