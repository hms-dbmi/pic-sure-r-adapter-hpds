# Test helpers that build a fake stand-in for the Python `picsure` module.
# Tests swap the package-private `picsure_py` binding to one of these fakes
# via `testthat::local_mocked_bindings()` so wrappers can be exercised
# without starting a Python process.
#
# Each fake method records the call for assertion.

`%||%` <- function(a, b) if (is.null(a)) b else a

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
        args <- list(...)
        calls$runQuery <- c(calls$runQuery, list(args))
        type <- tolower(args$type %||% "count")
        switch(
          type,
          count = 42L,
          participant = data.frame(
            patient_id = c(1L, 2L, 3L),
            value      = c("a", "b", "c"),
            stringsAsFactors = FALSE
          ),
          timestamp = data.frame(
            patient_id = c(1L, 2L),
            timestamp  = c("2026-01-01", "2026-01-02"),
            stringsAsFactors = FALSE
          ),
          stop("fake runQuery: unknown type '", type, "'")
        )
      },
      facets = function() new_fake_facet_set(),
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

# Fake FacetSet: a mutable Python-like object that records add/remove calls.
# Mirrors the minimal surface R wrappers touch: members `$add(key, value)`
# and `$remove(key, value)` that mutate internal state.
new_fake_facet_set <- function() {
  state <- new.env(parent = emptyenv())
  state$entries <- list()  # list of list(key = ..., value = ...)

  fs <- structure(
    list(
      add = function(key, value) {
        state$entries <- c(state$entries, list(list(key = key, value = value)))
        invisible(NULL)
      },
      remove = function(key, value) {
        keep <- vapply(
          state$entries,
          function(e) !(identical(e$key, key) && identical(e$value, value)),
          logical(1)
        )
        state$entries <- state$entries[keep]
        invisible(NULL)
      },
      .state = state
    ),
    class = "fake_facet_set"
  )
  fs
}
