# Public condition classes for errors surfaced from the picsure package.
#
# Two kinds of failure reach an R user, and both leave through here.
#
#   * A Python `PicSureError` raised inside the wrapped adapter. Reticulate
#     turns it into a condition whose message reads
#     "picsure.errors.PicSureAuthError: <the real message>" followed by
#     "Run `reticulate::py_last_error()` for details.", two layers of Python
#     plumbing around the sentence the Python package wrote for researchers.
#     `with_picsure_error()` unwraps it. The message becomes the
#     human-readable one alone, and the Python exception stays reachable on
#     the condition as `$py_cause` and `$python_class`.
#   * An argument the package rejected itself. Those used to be plain
#     `simpleError`s, so a handler written from the documented
#     `tryCatch(picsureError = ...)` missed them. They are now
#     `picsureValidationError`s, which are `picsureError`s.
#
# The class hierarchy mirrors the shape the Python adapter is moving to, so a
# handler written against either language reads the same once the pin catches
# up. It runs ahead of the pinned build in one place: pinned
# `PicSureConsentDeniedError` derives straight from `PicSureError`, while R
# already places `picsureConsentDeniedError` under `picsureAuthorizationError`
# and so under `picsureAuthError`. The R side owns its own ancestry. A mapped
# Python class is expanded through `.PICSURE_CONDITION_PARENTS` rather than
# by copying the installed Python build's MRO, so the R hierarchy keeps its
# shape when the pinned Python commit moves.

# Ancestors of each condition class, most specific first, excluding
# `picsureError`, which every one of them carries.
#
#   picsureError
#   |- picsureAuthError                  server answered and refused
#   |  |- picsureAuthenticationError     the token itself (401, or locally detected)
#   |  \- picsureAuthorizationError      token fine, account not permitted (403)
#   |     \- picsureConsentDeniedError   approved consents do not cover the data
#   |- picsureConnectionError            no usable response
#   |  |- picsureTLSError                certificate not trusted
#   |  \- picsureServerError             5xx
#   |     \- picsureConsentLookupError   server could not resolve consents (502)
#   |- picsureQueryError                 server rejected the query
#   \- picsureValidationError            invalid input to a picsure function
#
# PSAMA answers 403, not 401, for a bad or absent token on /user/me, so a
# server-side token rejection arrives as an authorization error while a
# locally-detected one (malformed, expired) is an authentication error. Both
# are `picsureAuthError`s, which is the class to catch when the fix is to
# refresh the token.
.PICSURE_CONDITION_PARENTS <- list(
  picsureAuthError           = character(),
  picsureAuthenticationError = "picsureAuthError",
  picsureAuthorizationError  = "picsureAuthError",
  picsureConsentDeniedError  = c("picsureAuthorizationError", "picsureAuthError"),
  picsureConnectionError     = character(),
  picsureTLSError            = "picsureConnectionError",
  picsureServerError         = "picsureConnectionError",
  picsureConsentLookupError  = c("picsureServerError", "picsureConnectionError"),
  picsureQueryError          = character(),
  picsureValidationError     = character()
)

# Python exception class to R condition class. Keys are the module-qualified
# names reticulate puts in the condition's class vector.
#
# The pinned Python build and the current Python source disagree on the shape
# of the hierarchy. The pinned one has a flat `PicSureAuthError` and defines
# none of `PicSureAuthenticationError`, `PicSureAuthorizationError`,
# `PicSureTLSError`, or `PicSureServerError`. Mapping the leaf class name and
# re-deriving the ancestry R-side means both builds produce a well-formed R
# condition. A pinned-build refusal is a `picsureAuthError`, and a newer
# build's refusal refines to the authentication or authorization subclass.
.PICSURE_PY_CONDITION_CLASSES <- c(
  "picsure.errors.PicSureConsentDeniedError"  = "picsureConsentDeniedError",
  "picsure.errors.PicSureConsentLookupError"  = "picsureConsentLookupError",
  "picsure.errors.PicSureAuthenticationError" = "picsureAuthenticationError",
  "picsure.errors.PicSureAuthorizationError"  = "picsureAuthorizationError",
  "picsure.errors.PicSureAuthError"           = "picsureAuthError",
  "picsure.errors.PicSureTLSError"            = "picsureTLSError",
  "picsure.errors.PicSureServerError"         = "picsureServerError",
  "picsure.errors.PicSureConnectionError"     = "picsureConnectionError",
  "picsure.errors.PicSureQueryError"          = "picsureQueryError",
  "picsure.errors.PicSureValidationError"     = "picsureValidationError"
)

# Backend `errorType` strings to R condition class, for a Python build whose
# exception class does not name the refinement. A fallback behind the
# class-vector lookup above.
.PICSURE_ERROR_TYPE_CLASSES <- c(
  consent_denied        = "picsureConsentDeniedError",
  consent_lookup_failed = "picsureConsentLookupError"
)

# Expands a condition class to the full vector `structure()` should carry.
#
# `class` must be NULL or one of the names in `.PICSURE_CONDITION_PARENTS`.
# Anything else is rejected by `match.arg()`, so a misspelled class in a
# wrapper fails at the raise rather than producing a condition no handler
# matches.
.picsure_condition_classes <- function(class = NULL) {
  base <- c("picsureError", "error", "condition")
  if (is.null(class)) {
    return(base)
  }
  class <- match.arg(class, names(.PICSURE_CONDITION_PARENTS))
  unique(c(class, .PICSURE_CONDITION_PARENTS[[class]], base))
}

#' Construct a picsureError condition.
#'
#' The base constructor for every error this package raises. `class` selects
#' one of the specialized condition classes, whose ancestors are filled in
#' automatically; the result always carries `picsureError`, so the documented
#' `tryCatch(picsureError = ...)` handler catches all of them.
#'
#' @param message The user-facing error message. When the source is a Python
#'   PicSureError, this is the Python-crafted message with reticulate's
#'   class prefix and `py_last_error()` footer removed. Python already wrote
#'   the sentence for researchers.
#' @param py_cause Optional; the condition reticulate raised for the original
#'   Python exception. Stored on the condition as `$py_cause` so the Python
#'   detail stays reachable (`reticulate::py_last_error()` still has the
#'   traceback) without appearing in the default message.
#' @param class Optional; one specialized condition class to carry in
#'   addition to `picsureError`. One of `"picsureAuthError"`,
#'   `"picsureAuthenticationError"`, `"picsureAuthorizationError"`,
#'   `"picsureConsentDeniedError"`, `"picsureConnectionError"`,
#'   `"picsureTLSError"`, `"picsureServerError"`,
#'   `"picsureConsentLookupError"`, `"picsureQueryError"`, or
#'   `"picsureValidationError"`. Ancestor classes are added for you. Any
#'   other value is an error.
#' @return A condition inheriting `c("picsureError", "error", "condition")`,
#'   with `$py_cause` and `$python_class` describing the Python origin when
#'   there was one.
#'
#' @section Condition hierarchy:
#' Every error this package raises inherits `picsureError`, so
#' `tryCatch(picsureError = ...)` still catches all of them. The subclasses
#' let you catch a kind of failure instead:
#'
#' \preformatted{
#' picsureError
#' |- picsureAuthError                  the server answered and refused
#' |  |- picsureAuthenticationError     the token itself (401, or detected here)
#' |  \- picsureAuthorizationError      token fine, account not permitted (403)
#' |     \- picsureConsentDeniedError   approved consents do not cover the data
#' |- picsureConnectionError            no usable response came back
#' |  |- picsureTLSError                the certificate was not trusted
#' |  \- picsureServerError             5xx
#' |     \- picsureConsentLookupError   server could not resolve consents (502)
#' |- picsureQueryError                 the server rejected the query
#' \- picsureValidationError            invalid input to a picsure function
#' }
#'
#' Catch `picsureAuthError` for "refresh the token and retry",
#' `picsureConnectionError` for "the deployment is unreachable, back off and
#' retry", `picsureQueryError` for "the query itself is wrong", and
#' `picsureValidationError` for an argument this package rejected before any
#' request was made. A token that is missing where one is required is a
#' `picsureValidationError`, the same as any other rejected argument. PSAMA
#' answers 403, not 401, for a bad token on `/user/me`, so a server-side
#' token rejection arrives as an authorization error while a locally-detected
#' one (malformed, expired) arrives as an authentication error. That is why
#' `picsureAuthError`, not either leaf, is the class to catch for token
#' trouble. Note that `picsureAuthError` also catches
#' `picsureConsentDeniedError`, which no token refresh will clear, so a
#' handler that refreshes and retries should test for
#' `picsureConsentDeniedError` first and give up on it rather than retry.
#'
#' **What the pinned Python build distinguishes.** The R side derives
#' ancestry from its own table rather than from the installed Python
#' exception's MRO, so the tree above is the shape of the R conditions on any
#' build. What varies is how finely the leaf is identified. The pinned Python
#' adapter has a flatter hierarchy. It defines `PicSureAuthError` but none of
#' `PicSureAuthenticationError`, `PicSureAuthorizationError`,
#' `PicSureTLSError`, or `PicSureServerError`. Against that build a
#' server-side refusal arrives as a plain `picsureAuthError` and a transport
#' failure as a plain `picsureConnectionError`. The authentication-versus-
#' authorization and TLS-versus-5xx splits start arriving once the pin moves
#' to a build that defines those classes. `picsureConsentDeniedError`,
#' `picsureConsentLookupError`, `picsureQueryError`, and
#' `picsureValidationError` are distinguished on the pinned build today, as
#' are the `picsureValidationError`s this package raises itself.
#' @examples
#' \dontrun{
#' tryCatch(
#'   picsure::connect(platform = "BDC Authorized", token = "bad-token"),
#'   picsureError = function(e) {
#'     message("PIC-SURE error: ", conditionMessage(e))
#'   }
#' )
#'
#' # An authentication problem specifically: refresh the token and retry.
#' tryCatch(
#'   picsure::searchDictionary(session, "sex"),
#'   picsureAuthError = function(e) message("Token problem: ", conditionMessage(e))
#' )
#' }
#' @export
picsureError <- function(message, py_cause = NULL, class = NULL) {
  structure(
    class = .picsure_condition_classes(class),
    list(
      message      = message,
      py_cause     = py_cause,
      python_class = .picsure_python_class(py_cause),
      call         = sys.call(-1L)
    )
  )
}

#' Raise the error for an argument this package rejected itself.
#'
#' Builds the condition and raises it in one step. A site written
#' `stop(picsureError(...))` forces its argument inside `stop`'s own frame,
#' so the condition records the `stop(...)` expression and the researcher
#' reads the constructor rather than the function they called. Raising from
#' here leaves the wrapper's frame one step up, so the error reports
#' `searchGenomicValues(...)`.
#'
#' @param message The user-facing message.
#' @param class The specialized condition class, `"picsureValidationError"`
#'   unless a caller names another. `NULL` for a plain `picsureError`.
#' @param call The call to report. Defaults to the caller's, which is the
#'   wrapper the researcher invoked when the check sits in that wrapper's own
#'   body. A nested closure or a shared check passes `NULL` instead, because
#'   its own frame is not a call the researcher made.
#' @return Never returns.
#' @noRd
.picsure_reject <- function(message, class = "picsureValidationError", call = sys.call(-1L)) {
  condition <- picsureError(message, class = class)
  condition$call <- call
  stop(condition)
}

# Reads the Python exception class name off a reticulate condition.
#
# Reticulate puts the exception's whole MRO in the condition's class vector,
# module-qualified and most specific first, for example
# c("picsure.errors.PicSureAuthError", "picsure.errors.PicSureError",
#   "python.builtin.Exception", ..., "error", "condition"). The first entry is
# the class that was raised. Returns NULL for anything that is not a Python
# condition.
.picsure_python_class <- function(py_cause) {
  if (is.null(py_cause)) {
    return(NULL)
  }
  classes <- class(py_cause)
  if (!any(startsWith(classes, "python.builtin."))) {
    return(NULL)
  }
  classes[[1L]]
}

.picsure_python_error_type <- function(py_cause) {
  error_type <- tryCatch(py_cause$error_type, error = function(e) NULL)
  if (is.null(error_type) || length(error_type) != 1L || !is.character(error_type)) {
    return(NULL)
  }
  error_type
}

# Picks the R condition class for a Python exception, most specific first.
.picsure_condition_class_for <- function(py_cause) {
  mapped <- .PICSURE_PY_CONDITION_CLASSES[class(py_cause)]
  mapped <- mapped[!is.na(mapped)]
  if (length(mapped) > 0L) {
    return(unname(mapped[[1L]]))
  }
  error_type <- .picsure_python_error_type(py_cause)
  if (is.null(error_type)) {
    return(NULL)
  }
  by_error_type <- .PICSURE_ERROR_TYPE_CLASSES[error_type]
  if (is.na(by_error_type)) NULL else unname(by_error_type)
}

# The footer reticulate appends to every Python error message.
.PICSURE_RETICULATE_FOOTER <- "Run `reticulate::py_last_error()` for details."

# Recovers the message the Python package wrote, without reticulate's framing.
#
# Prefers the exception's own `args[0]`, the string passed to the exception
# constructor, which carries no framing at all. Falls back to trimming the
# class prefix and the `py_last_error()` footer off `conditionMessage()`,
# which is all a condition with no reachable Python object offers, such as a
# hand-built one in a test.
.picsure_python_message <- function(py_cause) {
  direct <- tryCatch(
    {
      args <- py_cause$args
      if (length(args) >= 1L && is.character(args[[1L]]) &&
          length(args[[1L]]) == 1L && !is.na(args[[1L]])) {
        args[[1L]]
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )
  if (!is.null(direct) && nzchar(direct)) {
    return(direct)
  }
  .picsure_strip_reticulate_framing(
    .picsure_raw_message(py_cause),
    python_class = .picsure_python_class(py_cause)
  )
}

# Reads a Python condition's message without letting the read itself fail.
#
# `conditionMessage()` resolves `$message`, and `$` on anything classed
# `python.builtin.object` goes through reticulate, which throws for a
# condition whose Python object is missing or malformed. An error while
# building an error message would replace the researcher's message with
# reticulate's internals, so the plain list element is the fallback and an
# empty string the floor.
.picsure_raw_message <- function(py_cause) {
  usable <- function(x) is.character(x) && length(x) == 1L && !is.na(x)
  from_condition <- tryCatch(conditionMessage(py_cause), error = function(e) NULL)
  if (usable(from_condition)) {
    return(from_condition)
  }
  from_list <- tryCatch(unclass(py_cause)[["message"]], error = function(e) NULL)
  if (usable(from_list)) {
    return(from_list)
  }
  ""
}

# Strips reticulate's "<qualified class>: " prefix and trailing footer.
#
# The prefix is removed by exact match against the class that was raised, so
# a message whose own text happens to start with something colon-shaped is
# left alone.
.picsure_strip_reticulate_framing <- function(message, python_class = NULL) {
  if (length(message) != 1L || !is.character(message) || is.na(message)) {
    return(message)
  }
  if (!is.null(python_class)) {
    prefix <- paste0(python_class, ": ")
    if (startsWith(message, prefix)) {
      message <- substring(message, nchar(prefix) + 1L)
    }
  }
  lines <- strsplit(message, "\n", fixed = TRUE)[[1L]]
  keep <- !vapply(lines, function(l) identical(trimws(l), .PICSURE_RETICULATE_FOOTER),
                  logical(1), USE.NAMES = FALSE)
  trimws(paste(lines[keep], collapse = "\n"), which = "right")
}

# Appends the Python origin when the user asked to see it.
#
# `options(picsure.python_detail = TRUE)` puts the exception class and the
# `py_last_error()` pointer back into the message. Off by default, because an
# R user did not opt into Python and an ordinary refusal wrapped in Python
# plumbing reads like a bug in this package.
.picsure_with_python_detail <- function(message, py_cause) {
  if (!isTRUE(getOption("picsure.python_detail", FALSE))) {
    return(message)
  }
  python_class <- .picsure_python_class(py_cause)
  paste0(
    message,
    "\n(Python ", if (is.null(python_class)) "exception" else python_class,
    "; run reticulate::py_last_error() for the traceback.)"
  )
}

# Builds the picsureError for a Python exception caught at the boundary.
#
# The condition carries no `call`, so R prints the Python-crafted message on
# its own rather than prefixing it with the name of an internal helper.
# Recovering an enclosing frame instead would not be safe here:
# `with_picsure_error()` is invoked from places where the nearest frame is
# package plumbing and not the call the researcher wrote, inside
# `removeFacet()`'s restore `tryCatch` and inside the block form in
# `platforms()`, so a recovered call would sometimes name a `tryCatch`
# internal. The shared argument checkers pass `call = NULL` for the same
# reason, and the message Python wrote is written to stand on its own. The
# Python origin stays reachable on `$py_cause` and `$python_class`, and
# through `reticulate::py_last_error()`.
.picsure_error_from_python <- function(py_cause) {
  condition <- picsureError(
    .picsure_with_python_detail(.picsure_python_message(py_cause), py_cause),
    py_cause = py_cause,
    class    = .picsure_condition_class_for(py_cause)
  )
  condition$call <- NULL
  condition
}

#' Wrap a Python call so Python exceptions surface as picsureErrors.
#'
#' Any condition of class `python.builtin.Exception` thrown inside `expr` is
#' caught and re-raised as a `picsureError`, or as the specialized subclass
#' matching the Python exception's own class. The message is the Python
#' package's own sentence with reticulate's class prefix and
#' `py_last_error()` footer stripped; the original condition is attached as
#' `$py_cause` and the Python class name as `$python_class`. Set
#' `options(picsure.python_detail = TRUE)` to put the Python class back into
#' the message. Non-Python R errors pass through unchanged.
#'
#' @param expr An expression, typically a reticulate method call.
#' @return The value of `expr` if no error occurred.
#' @keywords internal
with_picsure_error <- function(expr) {
  tryCatch(
    expr,
    python.builtin.Exception = function(e) {
      stop(.picsure_error_from_python(e))
    }
  )
}
