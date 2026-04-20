# Test helpers that build a fake stand-in for the Python `picsure` module.
# Tests swap the package-private `picsure_py` binding to one of these fakes
# via `testthat::local_mocked_bindings()` so wrappers can be exercised
# without starting a Python process.
#
# Each fake method records the call for assertion.

new_fake_session <- function(platform = "Demo", token = "tok") {
  calls <- new.env(parent = emptyenv())
  calls$search <- list()
  calls$runQuery <- list()
  structure(
    list(
      platform = platform,
      token = token,
      user_id = "user_123",
      token_expires = "2026-05-14",
      search = function(...) {
        calls$search <- c(calls$search, list(list(...)))
        data.frame(
          name = character(), description = character(),
          stringsAsFactors = FALSE
        )
      },
      runQuery = function(...) {
        calls$runQuery <- c(calls$runQuery, list(list(...)))
        0L
      },
      .calls = calls
    ),
    class = "fake_session"
  )
}

fake_picsure_py <- function(platform_names = c("Demo", "BDC Open", "BDC Authorized")) {
  calls <- new.env(parent = emptyenv())
  calls$connect <- list()

  list(
    # Module-level callables
    connect = function(platform, token, ...) {
      calls$connect <- c(calls$connect, list(list(platform = platform, token = token, ...)))
      new_fake_session(platform = platform, token = token)
    },
    createClause = function(path, type, ...) {
      list(kind = "clause", path = path, type = type, extra = list(...))
    },
    buildClauseGroup = function(clauses, root) {
      list(kind = "group", clauses = clauses, root = root)
    },

    # Enums exposed as named lists so to_py_enum() and tests can look them up
    ClauseType = list(
      FILTER = "FILTER", SELECT = "SELECT",
      REQUIRE = "REQUIRE", ANYRECORD = "ANYRECORD"
    ),
    GroupOperator = list(AND = "AND", OR = "OR"),
    Platform = setNames(platform_names, toupper(gsub(" ", "_", platform_names))),

    # Call recorder
    .calls = calls
  )
}
