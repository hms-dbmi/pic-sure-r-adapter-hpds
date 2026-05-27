#' Connect to a PIC-SURE instance.
#'
#' Opens a session against the named PIC-SURE platform using the supplied
#' personal token. See [`Platform`][picsure::Platform] for the known
#' platform members.
#'
#' On the first call of the R session, reticulate provisions an isolated
#' Python environment containing the `picsure` Python package. This may take
#' a few seconds the first time; subsequent calls reuse the cached env.
#'
#' @param platform A `Platform` member (e.g.
#'   [`Platform$BDC_AUTHORIZED`][picsure::Platform]) or a full URL string
#'   for an unlisted deployment (e.g. `"https://my-picsure.example.com"`).
#'   Human-readable label strings such as `"BDC Authorized"` are **not**
#'   accepted and raise a `picsureError`.
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
#' bdc <- picsure::connect(platform = picsure::Platform$BDC_AUTHORIZED, token = my_token)
#' open <- picsure::connect(platform = picsure::Platform$BDC_OPEN)
#' }
#' @export
connect <- function(platform, token = "", ...) {
  bad_platform_msg <- "`platform` is required. Pass a Platform member (e.g. picsure::Platform$BDC_AUTHORIZED) or a full URL string. See ?picsure::Platform."
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
    # Only URL strings are valid string platforms. Python's
    # resolve_platform rejects human-readable labels ("BDC Authorized"),
    # so catch them here with an R-side message rather than letting the
    # forwarded call fail deeper with a less specific error.
    if (!grepl("^https?://", platform, ignore.case = TRUE)) {
      stop(picsureError(sprintf(
        paste0(
          "%s is not a valid platform. Platform labels are not accepted; ",
          "pass a Platform member (e.g. picsure::Platform$BDC_AUTHORIZED) ",
          "or a full URL string (e.g. \"https://my-picsure.example.com\"). ",
          "See ?picsure::Platform."
        ),
        encodeString(platform, quote = "\"")
      )))
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
