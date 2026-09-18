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
#'   string resolve differently. See the "Defaults" section below.
#'   Supported keys:
#'   \describe{
#'     \item{`include_consents`}{Logical. When `TRUE`, `connect()` fetches the
#'       user's approved consent identifiers from PSAMA and the session sends
#'       them on every dictionary call, which is what scopes search results to
#'       the studies the user may see. `TRUE` for the BDC authorized
#'       `Platform` members, `FALSE` for every other member **and for a
#'       custom URL string**.}
#'     \item{`requires_auth`}{Logical. When `FALSE`, neither this package nor
#'       the Python adapter asks for a token, even on an `_AUTHORIZED`
#'       member. It does not on its own move the session to the open HPDS
#'       backend: the pinned adapter picks that backend only when neither
#'       `requires_auth` nor `include_consents` is in force, and
#'       `include_consents` still defaults to the member's own flag, which is
#'       `TRUE` for the BDC authorized members. To connect anonymously
#'       against the open backend on such a member, pass
#'       `include_consents = FALSE` as well; otherwise `connect()` calls
#'       PSAMA's consent endpoint with no token. When `TRUE`, a token is
#'       required, even for a custom URL string. Defaults to `TRUE` for the
#'       `_AUTHORIZED` members and for a custom URL string, `FALSE` for the
#'       `_OPEN` members.}
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
#'     \item{`timeout`}{**The currently pinned Python adapter does not accept
#'       this argument.** Passing it fails with an unexpected-keyword error
#'       until the pin moves. Once the pin moves, it is a number: the
#'       per-request deadline in seconds for the data operations the session
#'       performs, counts, participant downloads, and export polls. It will
#'       default to the Python adapter's ten minutes, because a large dataset
#'       can take minutes to assemble server-side, and the connect-time
#'       validation request will keep its own short deadline.}
#'     \item{`validate`}{**The currently pinned Python adapter does not accept
#'       this argument.** Passing it fails with an unexpected-keyword error
#'       until the pin moves. Once the pin moves, it is a logical: `TRUE`,
#'       the Python adapter's default, will make `connect()` check the
#'       token's shape and expiry locally and then send one request to
#'       confirm the deployment is reachable and accepts the token, and
#'       `FALSE` will skip both for offline or mocked use, so that nothing is
#'       sent or checked and the returned session may not work.}
#'   }
#'   `timeout` and `validate` are accepted here ahead of the pin bump that
#'   makes them usable. Until then either one reaches Python and comes back
#'   as a `picsureError` reporting an unexpected keyword argument, not as a
#'   `picsureValidationError`.
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
#' has not started yet. After the first call that provisions it, the change
#' is invisible to Python. Use the R options instead, which are read on every
#' `connect()` call:
#'
#' \preformatted{
#' options(picsure.ssl_verify = FALSE)   # or a path to a CA bundle
#' options(picsure.dev_mode  = TRUE)
#' }
#'
#' An explicit `verify =` / `dev_mode =` argument to `connect()` always wins
#' over the option, and both go through the same checks: `dev_mode` must be
#' a single `TRUE` or `FALSE`, and `verify` must be `TRUE`, `FALSE`, or a
#' path string. A string that spells a boolean, such as `"false"`, is
#' rejected rather than treated as a certificate path. When the `verify`
#' argument or `options(picsure.ssl_verify)` turns verification off,
#' `connect()` emits a message naming which of the two did it. Verification
#' turned off Python-side through `PICSURE_SSL_VERIFY` is silent on the R
#' side, one more reason to prefer the option.
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
    .picsure_reject(bad_platform_msg)
  }
  # Track whether the platform is identifiably auth-required from R-side
  # info alone. For label strings or raw Python objects we can't tell
  # here and defer to Python's resolve_platform check.
  requires_auth_known <- inherits(platform, "picsure_platform") &&
                         isTRUE(platform$requires_auth)
  if (inherits(platform, "picsure_enum_member")) {
    if (!inherits(platform, "picsure_platform")) {
      .picsure_reject(
        sprintf("Expected a Platform member, got %s.", format(platform))
      )
    }
    # Convert the R-side member to the Python Platform enum member by
    # name. Going through the proxy avoids the URL/label ambiguity:
    # BDC_AUTHORIZED and BDC_OPEN share a URL (distinguished by the
    # /hpds/auth vs /hpds/open backend path); BDC_AUTHORIZED and
    # BDC_DEV_AUTHORIZED share a label.
    platform <- picsure_py$Platform[[platform$name]]
  } else if (is.character(platform)) {
    if (length(platform) != 1L || is.na(platform) || !nzchar(platform)) {
      .picsure_reject(bad_platform_msg)
    }
    # Only URL strings are valid string platforms. Python's
    # resolve_platform rejects human-readable labels ("BDC Authorized"),
    # so catch them here with an R-side message rather than letting the
    # forwarded call fail deeper with a less specific error.
    if (!grepl("^https?://", platform, ignore.case = TRUE)) {
      .picsure_reject(
        sprintf(
          paste0(
            "%s is not a valid platform. Platform labels are not accepted; ",
            "pass a Platform member (e.g. picsure::Platform$BDC_AUTHORIZED) ",
            "or a full URL string (e.g. \"https://my-picsure.example.com\"). ",
            "See ?picsure::Platform."
          ),
          encodeString(platform, quote = "\"")
        )
      )
    }
  } else if (!inherits(platform, "python.builtin.object")) {
    # Reject logicals like NA, numerics, lists, etc.; accept only strings,
    # picsure_platform members, or Python objects (e.g. a reticulate-
    # wrapped Platform enum member).
    .picsure_reject(bad_platform_msg)
  }
  extras <- list(...)
  unknown <- setdiff(names(extras), CONNECT_EXTRA_KWARGS)
  if (length(unknown) > 0) {
    .picsure_reject(
      sprintf(
        "Unknown argument(s) to connect(): %s. Valid extras: %s.",
        paste(sprintf("`%s`", unknown), collapse = ", "),
        paste(sprintf("`%s`", CONNECT_EXTRA_KWARGS), collapse = ", ")
      )
    )
  }

  if (!is.null(extras$requires_auth)) {
    requires_auth_known <- isTRUE(extras$requires_auth)
  }
  # Token type validation runs regardless of platform. The non-empty
  # check fires only when the R-side platform info confirms auth is
  # required; otherwise Python's own check (which sees the resolved
  # PlatformInfo) decides.
  if (is.null(token)) token <- ""
  if (!is.character(token) || length(token) != 1L || is.na(token)) {
    .picsure_reject(
      "`token` must be a single character string (use \"\" for open platforms)."
    )
  }
  if (requires_auth_known && !nzchar(token)) {
    .picsure_reject(
      "`token` is required for this platform. Copy it from the 'User Profile' tab of PIC-SURE."
    )
  }

  # Identify this adapter to the backend audit log. Python's connect()
  # defaults to "PYTHON_ADAPTER"; override it here unless the caller set it.
  if (is.null(extras$client_type)) extras$client_type <- "R_ADAPTER"

  verify <- .picsure_setting(
    extras$verify, "verify", "picsure.ssl_verify", .picsure_check_verify
  )
  extras$verify <- verify$value
  if (isFALSE(verify$value)) {
    message(sprintf(
      paste0(
        "TLS certificate verification is off for this connection, set by %s. ",
        "Use this only for local or self-signed deployments."
      ),
      verify$source
    ))
  }
  dev_mode <- .picsure_setting(
    extras$dev_mode, "dev_mode", "picsure.dev_mode", .picsure_check_flag
  )
  extras$dev_mode <- dev_mode$value

  kwargs <- drop_nulls(c(
    list(platform = platform, token = token),
    extras
  ))

  with_picsure_error(do.call(picsure_py$connect, kwargs))
}

#' Resolve a connect() setting from its argument or its R option.
#'
#' An explicit argument wins. Otherwise the R option is read, on every call,
#' and `NULL` means neither was given, so the Python adapter's own default
#' stands. The options exist because `PICSURE_SSL_VERIFY` and
#' `PICSURE_DEV_MODE` are read by the Python adapter through `os.environ`,
#' which CPython snapshots when the interpreter starts. Reticulate embeds
#' that interpreter in the R process, so `Sys.setenv()` reaches Python only
#' before the first call has provisioned it.
#'
#' @param explicit The value passed to `connect()`, or `NULL`.
#' @param arg_name The `connect()` argument name, used in messages.
#' @param option_name The R option name, for example `"picsure.ssl_verify"`.
#' @param check Validator called as `check(value, label)`. It returns the
#'   value or signals a `picsureValidationError` that names `label`.
#' @return A list with `value`, the validated setting or `NULL`, and
#'   `source`, a phrase naming where it came from, or `NULL` when unset.
#' @noRd
.picsure_setting <- function(explicit, arg_name, option_name, check) {
  if (!is.null(explicit)) {
    label <- sprintf("`%s`", arg_name)
    return(list(value = check(explicit, label), source = sprintf("the %s argument", label)))
  }
  value <- getOption(option_name, NULL)
  if (is.null(value)) {
    return(list(value = NULL, source = NULL))
  }
  label <- sprintf("options(%s)", option_name)
  list(value = check(value, label), source = label)
}

#' Strings the Python adapter reads as booleans in `PICSURE_SSL_VERIFY`.
#'
#' The R side rejects them instead of forwarding them, because an explicit
#' `verify` string reaches httpx as a certificate path.
#'
#' These two vectors mirror inline Python literals, not module constants.
#' Both word lists are tuples written into the body of `_resolve_verify()` in
#' `picsure/_transport/client.py`, so there is no importable name to compare
#' them against and no live drift guard is possible, unlike
#' `CONNECT_EXTRA_KWARGS` and the enums, which read the pinned build. Read
#' that Python function by hand when the pin moves. A word Python starts
#' accepting and R does not know is forwarded rather than rejected, httpx
#' then treats it as a CA-bundle path, and a researcher asking to turn
#' verification off gets a certificate-load error instead.
#' @noRd
.VERIFY_TRUE_STRINGS <- c("true", "1", "yes", "on")
.VERIFY_FALSE_STRINGS <- c("false", "0", "no", "off")

#' Validate a `verify` setting.
#'
#' @param value `TRUE`, `FALSE`, or a path to a CA bundle as a single string.
#'   A string that spells a boolean is rejected with a message saying which
#'   logical to pass instead.
#' @param label Name of the argument or option, used in the error message.
#' @return `value` unchanged.
#' @noRd
.picsure_check_verify <- function(value, label) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }
  if (is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)) {
    spelled <- tolower(trimws(value))
    if (spelled %in% c(.VERIFY_TRUE_STRINGS, .VERIFY_FALSE_STRINGS)) {
      .picsure_reject(sprintf(
        paste0(
          "%s must be TRUE, FALSE, or a path to a CA bundle; got the string %s. ",
          "Pass the logical %s instead."
        ),
        label, encodeString(value, quote = "\""),
        if (spelled %in% .VERIFY_TRUE_STRINGS) "TRUE" else "FALSE"
      ), call = NULL)
    }
    return(value)
  }
  .picsure_reject(sprintf(
    paste0(
      "%s must be TRUE, FALSE, or a path to a CA bundle as a single ",
      "string; got %s."
    ),
    label, describe_argument_value(value)
  ), call = NULL)
}

#' Validate a logical flag setting.
#'
#' @param value `TRUE` or `FALSE`.
#' @param label Name of the argument or option, used in the error message.
#' @return `value` unchanged.
#' @noRd
.picsure_check_flag <- function(value, label) {
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(value)
  }
  .picsure_reject(sprintf(
    "%s must be TRUE or FALSE; got %s.", label,
    describe_argument_value(value)
  ), call = NULL)
}

# Whitelist of optional kwargs forwarded through `...` to picsure_py$connect.
# Names mirror the Python adapter's snake_case kwargs 1:1 — no R-side
# translation. To bump: add the new kwarg here and to connect()'s @param block.
CONNECT_EXTRA_KWARGS <- c(
  "include_consents", "requires_auth", "supports_genomic",
  "client_type", "dev_mode", "verify", "timeout", "validate"
)
