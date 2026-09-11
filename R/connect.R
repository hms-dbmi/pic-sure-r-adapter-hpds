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
#'   list of valid keys. Every one of them defaults to the *platform's* own
#'   setting rather than to a fixed value, and a `Platform` member and a URL
#'   string resolve differently — see the "Defaults" section below.
#'   Supported keys:
#'   \describe{
#'     \item{`include_consents`}{Logical. When `TRUE`, `connect()` fetches the
#'       user's approved consent identifiers from PSAMA and the session sends
#'       them on every dictionary call, which is what scopes search results to
#'       the studies the user may see. `TRUE` for the BDC authorized
#'       `Platform` members, `FALSE` for every other member **and for a
#'       custom URL string**.}
#'     \item{`requires_auth`}{Logical. When `FALSE`, the session is opened in
#'       unauthenticated mode against the open HPDS backend (no token
#'       needed). `TRUE` for the `_AUTHORIZED` members and for a custom URL
#'       string, `FALSE` for the `_OPEN` members.}
#'     \item{`supports_genomic`}{Logical. Whether genomic operations
#'       (`searchGenomicValues()`, genomic-filtered queries) are permitted.
#'       `TRUE` for `Platform$BDC_AUTHORIZED`, `Platform$BDC_DEV_AUTHORIZED`,
#'       `Platform$BDC_PREDEV_AUTHORIZED`, and
#'       `Platform$NHANES_AUTHORIZED`; `FALSE` for the `_OPEN` members and
#'       for a custom URL string. Pass `TRUE` for a custom URL that serves
#'       genomic data.}
#'     \item{`client_type`}{Identifier sent to the backend audit log as the
#'       `X-Client-Type` header. Defaults to `"R_ADAPTER"` (this wrapper sets
#'       it; the Python adapter's own default is `"PYTHON_ADAPTER"`). You
#'       should not normally need to override it.}
#'     \item{`dev_mode`}{Logical. Enables the Python adapter's developer-mode
#'       instrumentation (per-call event capture). Defaults to
#'       `getOption("picsure.dev_mode")`, then to the `PICSURE_DEV_MODE`
#'       environment variable read Python-side, then to off.}
#'     \item{`verify`}{TLS certificate verification. `TRUE` verifies (the
#'       Python adapter's default); `FALSE` disables it (self-signed /
#'       local-dev deployments only); a string is treated as a path to a CA
#'       bundle. Defaults to `getOption("picsure.ssl_verify")`, then to the
#'       `PICSURE_SSL_VERIFY` environment variable read Python-side, then to
#'       verifying.}
#'   }
#'
#' @section Defaults:
#' `include_consents`, `requires_auth`, and `supports_genomic` are all
#' `NULL` here and resolved Python-side by `resolve_platform()`. There is no
#' single default value for any of them:
#'
#' - For a `Platform` member, each one defaults to that member's own flag.
#'   `Platform$BDC_AUTHORIZED` is consent-scoped, auth-required, and
#'   genomic-capable; `Platform$NHANES_AUTHORIZED` is auth-required and
#'   genomic-capable but **not** consent-scoped; the `_OPEN` members are none
#'   of the three.
#' - For a **custom URL string**, `requires_auth` defaults to `TRUE` while
#'   `include_consents` and `supports_genomic` default to `FALSE`.
#'
#' The URL case is the one that bites. A consent-gated deployment reached by
#' URL connects with an **empty consent list**, and because the consent list
#' is what scopes dictionary results, `searchDictionary()` then returns every
#' concept in the index rather than the ones your consents cover. Pass
#' `include_consents = TRUE` explicitly when connecting to a consent-gated
#' deployment by URL:
#'
#' \preformatted{
#' session <- picsure::connect(
#'   platform         = "https://my-picsure.example.com",
#'   token            = my_token,
#'   include_consents = TRUE
#' )
#' }
#'
#' @section Settings from an R session:
#' `PICSURE_SSL_VERIFY` and `PICSURE_DEV_MODE` are read out of Python's
#' `os.environ`, which CPython snapshots when the interpreter starts.
#' Reticulate runs that interpreter inside the R process, so
#' `Sys.setenv(PICSURE_SSL_VERIFY = "false")` is picked up only while Python
#' has not started yet — after the first call that provisions it, the change
#' is invisible to Python. Use the R options instead, which are read on every
#' `connect()` call:
#'
#' \preformatted{
#' options(picsure.ssl_verify = FALSE)   # or a path to a CA bundle
#' options(picsure.dev_mode  = TRUE)
#' }
#'
#' An explicit `verify =` / `dev_mode =` argument to `connect()` always wins
#' over the option.
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
    stop(picsureError(bad_platform_msg, class = "picsureValidationError"))
  }
  # Track whether the platform is identifiably auth-required from R-side
  # info alone. For label strings or raw Python objects we can't tell
  # here and defer to Python's resolve_platform check.
  requires_auth_known <- inherits(platform, "picsure_platform") &&
                         isTRUE(platform$requires_auth)
  if (inherits(platform, "picsure_enum_member")) {
    if (!inherits(platform, "picsure_platform")) {
      stop(picsureError(
        sprintf("Expected a Platform member, got %s.", format(platform)),
        class = "picsureValidationError"
      ))
    }
    # Convert the R-side member to the Python Platform enum member by
    # name. Going through the proxy avoids the URL/label ambiguity:
    # BDC_AUTHORIZED and BDC_OPEN share a URL (distinguished by the
    # /hpds/auth vs /hpds/open backend path); BDC_AUTHORIZED and
    # BDC_DEV_AUTHORIZED share a label.
    platform <- picsure_py$Platform[[platform$name]]
  } else if (is.character(platform)) {
    if (length(platform) != 1L || is.na(platform) || !nzchar(platform)) {
      stop(picsureError(bad_platform_msg, class = "picsureValidationError"))
    }
    # Only URL strings are valid string platforms. Python's
    # resolve_platform rejects human-readable labels ("BDC Authorized"),
    # so catch them here with an R-side message rather than letting the
    # forwarded call fail deeper with a less specific error.
    if (!grepl("^https?://", platform, ignore.case = TRUE)) {
      stop(picsureError(
        sprintf(
          paste0(
            "%s is not a valid platform. Platform labels are not accepted; ",
            "pass a Platform member (e.g. picsure::Platform$BDC_AUTHORIZED) ",
            "or a full URL string (e.g. \"https://my-picsure.example.com\"). ",
            "See ?picsure::Platform."
          ),
          encodeString(platform, quote = "\"")
        ),
        class = "picsureValidationError"
      ))
    }
  } else if (!inherits(platform, "python.builtin.object")) {
    # Reject logicals like NA, numerics, lists, etc.; accept only strings,
    # picsure_platform members, or Python objects (e.g. a reticulate-
    # wrapped Platform enum member).
    stop(picsureError(bad_platform_msg, class = "picsureValidationError"))
  }
  # Token type validation runs regardless of platform. The non-empty
  # check fires only when the R-side platform info confirms auth is
  # required; otherwise Python's own check (which sees the resolved
  # PlatformInfo) decides.
  if (is.null(token)) token <- ""
  if (!is.character(token) || length(token) != 1L || is.na(token)) {
    stop(picsureError(
      "`token` must be a single character string (use \"\" for open platforms).",
      class = "picsureValidationError"
    ))
  }
  if (requires_auth_known && !nzchar(token)) {
    stop(picsureError(
      "`token` is required for this platform. Copy it from the 'User Profile' tab of PIC-SURE.",
      class = "picsureAuthenticationError"
    ))
  }

  extras <- list(...)
  unknown <- setdiff(names(extras), CONNECT_EXTRA_KWARGS)
  if (length(unknown) > 0) {
    stop(picsureError(
      sprintf(
        "Unknown argument(s) to connect(): %s. Valid extras: %s.",
        paste(sprintf("`%s`", unknown), collapse = ", "),
        paste(sprintf("`%s`", CONNECT_EXTRA_KWARGS), collapse = ", ")
      ),
      class = "picsureValidationError"
    ))
  }

  # Identify this adapter to the backend audit log. Python's connect()
  # defaults to "PYTHON_ADAPTER"; override it here unless the caller set it.
  if (is.null(extras$client_type)) extras$client_type <- "R_ADAPTER"

  if (is.null(extras$verify)) {
    extras$verify <- .picsure_option_default("picsure.ssl_verify", .picsure_check_verify)
  }
  if (is.null(extras$dev_mode)) {
    extras$dev_mode <- .picsure_option_default("picsure.dev_mode", .picsure_check_flag)
  }

  kwargs <- drop_nulls(c(
    list(platform = platform, token = token),
    extras
  ))

  with_picsure_error(do.call(picsure_py$connect, kwargs))
}

# Reads an R option and validates it, or returns NULL when it is unset.
#
# `PICSURE_SSL_VERIFY` and `PICSURE_DEV_MODE` are read by the Python adapter
# through `os.environ`, which CPython snapshots into a dict when the
# interpreter starts and never refreshes. Reticulate embeds that interpreter
# in the R process, so `Sys.setenv()` reaches Python only while Python has not
# started yet -- once the first call has provisioned it, a later
# `Sys.setenv(PICSURE_SSL_VERIFY = "false")` changes the process environment
# and Python does not see it. (The Python adapter itself reads the variables at
# call time, not at import; the interpreter's start is the boundary, not
# `import picsure`.)
#
# These options are the R-level equivalent, and they are read here, per call,
# so setting one takes effect on the next connect() regardless of when the
# interpreter came up.
.picsure_option_default <- function(name, check) {
  value <- getOption(name, NULL)
  if (is.null(value)) {
    return(NULL)
  }
  check(value, name)
}

# TLS verification: TRUE/FALSE, or a path to a CA bundle.
.picsure_check_verify <- function(value, name) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }
  if (is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)) {
    return(value)
  }
  stop(.picsure_invalid_argument(sprintf(
    paste0(
      "options(%s) must be TRUE, FALSE, or a path to a CA bundle as a single ",
      "string; got %s."
    ),
    name, describe_argument_value(value)
  )))
}

.picsure_check_flag <- function(value, name) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }
  stop(.picsure_invalid_argument(sprintf(
    "options(%s) must be TRUE or FALSE; got %s.", name,
    describe_argument_value(value)
  )))
}

# Whitelist of optional kwargs forwarded through `...` to picsure_py$connect.
# Names mirror the Python adapter's snake_case kwargs 1:1 — no R-side
# translation. To bump: add the new kwarg here and to connect()'s @param block.
CONNECT_EXTRA_KWARGS <- c(
  "include_consents", "requires_auth", "supports_genomic",
  "client_type", "dev_mode", "verify"
)
