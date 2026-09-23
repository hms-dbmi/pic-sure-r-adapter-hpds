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
#'   accepted and raise a `picsureError`. A member is resolved against the
#'   Python enum by its member name, which is the only unambiguous handle:
#'   `Platform$BDC_AUTHORIZED` and `Platform$BDC_DEV_AUTHORIZED` share a
#'   label, and `Platform$BDC_AUTHORIZED` and `Platform$BDC_OPEN` share a
#'   URL, told apart by the `/hpds/auth` and `/hpds/open` backend paths.
#' @param token Your personal PIC-SURE access token, obtained from the
#'   "User Profile" tab of your PIC-SURE instance. Optional for open-access
#'   platforms (`Platform$BDC_OPEN`, `Platform$BDC_DEV_OPEN`,
#'   `Platform$BDC_PREDEV_OPEN`, `Platform$NHANES_OPEN`); required for
#'   authenticated platforms. Defaults to `""`.
#' @param ... Optional keyword arguments forwarded to the Python
#'   `picsure.connect()` call. Unknown keys raise a `picsureError` with the
#'   list of valid keys. `include_consents`, `requires_auth`, and
#'   `supports_genomic` default to the *platform's* own setting rather than
#'   to a fixed value, and a `Platform` member and a URL string resolve
#'   differently. See the "Defaults" section below.
#'   Supported keys:
#'   \describe{
#'     \item{`include_consents`}{Logical. When `TRUE`, `connect()` fetches the
#'       user's approved consent identifiers from PSAMA and the session sends
#'       them on every dictionary call, which is what scopes search results to
#'       the studies the user may see. `TRUE` for the BDC authorized
#'       `Platform` members and `FALSE` for every other member. For a custom
#'       URL string it is detected rather than fixed; see the "Defaults"
#'       section below. It cannot be `TRUE` together with
#'       `requires_auth = FALSE`, because the consent list is read from an
#'       authenticated PSAMA endpoint.}
#'     \item{`requires_auth`}{Logical. When `FALSE`, neither this package nor
#'       the Python adapter asks for a token, and the session uses the open
#'       HPDS backend. On a consent-scoped member such as
#'       `Platform$BDC_AUTHORIZED`, `include_consents` still defaults to the
#'       member's own `TRUE`, and the Python adapter refuses that combination
#'       with a `picsureValidationError`, so pass `include_consents = FALSE`
#'       as well to connect anonymously there. When `TRUE`, a token is
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
#'     \item{`timeout`}{A single positive number of seconds. Defaults to the
#'       Python adapter's ten minutes, because a large dataset can take
#'       minutes to assemble server-side. It is the deadline for each request
#'       the session makes for a data operation, such as a count or a
#'       download. Participant and timestamp queries and
#'       [`exportAsPFB()`][picsure::exportAsPFB] run as server jobs, and for
#'       those it is also the overall budget for submitting the job and
#'       waiting on it, measured from just before the submit. The final
#'       status poll and the download after it each get their own deadline
#'       of the same length, so such a call can still run past `timeout` end
#'       to end. A job still running when the budget is spent raises a
#'       `picsureConnectionError`. The connect-time check below keeps its own
#'       deadline of 15 seconds, so a mistyped host fails fast whatever
#'       `timeout` is.}
#'     \item{`validate`}{A single `TRUE` or `FALSE`. `TRUE`, the default,
#'       makes `connect()` check the token's shape and expiry locally and
#'       then send one `GET /psama/user/me` request to confirm the deployment
#'       is reachable and accepts the token. `FALSE` skips those checks, that
#'       request, and the consent-scoping probe a custom URL string would
#'       otherwise get, for offline or mocked use, so the returned session
#'       may not work. A consent-scoped platform still fetches its consent
#'       list either way.}
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
#' - For a **custom URL string**, `requires_auth` defaults to `TRUE` and
#'   `supports_genomic` to `FALSE`. `include_consents` has no fixed default:
#'   when you pass none, and a token is required and `validate` is `TRUE`,
#'   `connect()` asks PSAMA for the account's consents and turns scoping on
#'   if any come back, saying so in a message.
#'
#' The URL case is the one that can still bite. When the probe cannot answer,
#' because `validate = FALSE`, PSAMA does not serve the consents route, or
#' the account has no consents, the session connects with an **empty
#' consent list** and the Python adapter prints a warning naming
#' `include_consents=True`. Because the consent list is what scopes
#' dictionary results, `searchDictionary()` then returns every concept in the
#' index rather than the ones your consents cover. Pass
#' `include_consents = TRUE` explicitly when connecting to a consent-gated
#' deployment by URL, which fetches the list without relying on the probe:
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
    platform <- to_py_enum(platform, picsure_py$Platform, "Platform",
                           "picsure_platform")
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
    extras$dev_mode, "dev_mode", "picsure.dev_mode", as_single_flag
  )
  extras$dev_mode <- dev_mode$value
  if (!is.null(extras$timeout)) {
    extras$timeout <- .picsure_check_timeout(extras$timeout)
  }
  if (!is.null(extras$validate)) {
    extras$validate <- as_single_flag(extras$validate, "`validate`")
  }

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
#' @param check Validator called as `check(value, label, call)`. It returns
#'   the value or signals a `picsureValidationError` that names `label`.
#' @param call The call to report in a rejection, by default this function's
#'   own caller. Threaded down to `check` because the validator's caller is
#'   this helper rather than `connect()`, so leaving it to the validator's
#'   own default would report an internal.
#' @return A list with `value`, the validated setting or `NULL`, and
#'   `source`, a phrase naming where it came from, or `NULL` when unset.
#' @noRd
.picsure_setting <- function(explicit, arg_name, option_name, check,
                             call = sys.call(-1L)) {
  if (!is.null(explicit)) {
    label <- sprintf("`%s`", arg_name)
    return(list(
      value  = check(explicit, label, call = call),
      source = sprintf("the %s argument", label)
    ))
  }
  value <- getOption(option_name, NULL)
  if (is.null(value)) {
    return(list(value = NULL, source = NULL))
  }
  label <- sprintf("options(%s)", option_name)
  list(value = check(value, label, call = call), source = label)
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
#' @param call The call to report in the error. `.picsure_setting()` passes
#'   `connect()`'s own call, because this validator's caller is that helper.
#' @return `value` unchanged.
#' @noRd
.picsure_check_verify <- function(value, label, call = sys.call(-1L)) {
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
      ), call = call)
    }
    return(value)
  }
  .picsure_reject(sprintf(
    paste0(
      "%s must be TRUE, FALSE, or a path to a CA bundle as a single ",
      "string; got %s."
    ),
    label, describe_argument_value(value)
  ), call = call)
}

#' Validate a `timeout` passed to `connect()`.
#'
#' The Python adapter checks neither the type nor the sign of `timeout` at
#' connect time, so a string, a negative number, or zero would be accepted
#' there and fail only at the first data request, inside the HTTP client.
#' Checking here rejects it before anything is sent.
#'
#' @param value The `timeout` argument, already known not to be `NULL`.
#' @param call The call to report in the error, by default `connect()`'s.
#' @return `value` as a double scalar, so Python receives a `float` whether
#'   the caller passed `60` or `60L`.
#' @noRd
.picsure_check_timeout <- function(value, call = sys.call(-1L)) {
  check_optional_number(value, "timeout", call = call)
  if (value <= 0) {
    .picsure_reject(
      sprintf(
        "`timeout` must be a positive number of seconds; got %s.",
        describe_argument_value(value)
      ),
      call = call
    )
  }
  as.double(value)
}

# Whitelist of optional kwargs forwarded through `...` to picsure_py$connect.
# Names mirror the Python adapter's snake_case kwargs 1:1, no R-side
# translation. Every name here must be one the pinned `picsure.connect()`
# accepts: the list is what `connect()` prints as "Valid extras", and a name
# Python refuses turns a clean R-side refusal into a Python unexpected-keyword
# error. `test-connect-reticulate.R` reads the pinned signature and checks
# both directions. To bump: add the new kwarg here and to connect()'s @param
# block.
CONNECT_EXTRA_KWARGS <- c(
  "include_consents", "requires_auth", "supports_genomic",
  "client_type", "dev_mode", "verify", "timeout", "validate"
)
