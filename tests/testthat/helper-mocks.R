# Test helpers that build a fake stand-in for the Python `picsure` module.
# Tests swap the package-private `picsure_py` binding to one of these fakes
# via `testthat::local_mocked_bindings()` so wrappers can be exercised
# without starting a Python process.
#
# Each fake method records the call for assertion.

`%||%` <- function(a, b) if (is.null(a)) b else a

new_fake_session <- function(platform = "Demo", token = "tok") {
  calls <- new.env(parent = emptyenv())
  calls$searchDictionary <- list()
  calls$runQuery <- list()
  calls$exportAsPFB <- list()
  calls$exportCSV <- list()
  calls$exportTSV <- list()
  calls$loadQueryByID <- list()
  calls$runQueryByID <- list()
  calls$saveQueryByName <- list()
  structure(
    list(
      platform = platform,
      token = token,
      user_id = "user_123",
      token_expires = "2026-05-14",
      searchDictionary = function(...) {
        calls$searchDictionary <- c(calls$searchDictionary, list(list(...)))
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
          count = list(value = 42L, margin = 0L, cap = NULL),
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
      exportAsPFB = function(query, path) {
        calls$exportAsPFB <- c(calls$exportAsPFB, list(list(query = query, path = path)))
        writeLines(character(0), path)
        invisible(NULL)
      },
      exportCSV = function(data, path) {
        calls$exportCSV <- c(calls$exportCSV, list(list(data = data, path = path)))
        writeLines(character(0), path)
        invisible(NULL)
      },
      exportTSV = function(data, path) {
        calls$exportTSV <- c(calls$exportTSV, list(list(data = data, path = path)))
        writeLines(character(0), path)
        invisible(NULL)
      },
      facets = function() new_fake_facet_set(),
      loadQueryByID = function(query_id) {
        calls$loadQueryByID <- c(calls$loadQueryByID, list(list(query_id = query_id)))
        list(kind = "group", clauses = list(), operator = "AND",
             loaded_from = query_id)
      },
      runQueryByID = function(query_id, type = "count") {
        calls$runQueryByID <- c(
          calls$runQueryByID,
          list(list(query_id = query_id, type = type))
        )
        t <- tolower(type %||% "count")
        switch(
          t,
          count = list(value = 42L, margin = 0L, cap = NULL,
                       loaded_from = query_id),
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
          stop("fake runQueryByID: unknown type '", t, "'")
        )
      },
      saveQueryByName = function(query, name, overwrite = FALSE) {
        calls$saveQueryByName <- c(
          calls$saveQueryByName,
          list(list(query = query, name = name, overwrite = overwrite))
        )
        "qid-fake-001"
      },
      .calls = calls
    ),
    class = "fake_session"
  )
}

# Fake stand-in for the `picsure_py` module surface used by the tree-edit
# wrappers (removeSubQuery, replaceClause). Records each call so tests can
# assert on what was forwarded.
new_fake_picsure_py <- function() {
  calls <- new.env(parent = emptyenv())
  calls$removeSubQuery <- list()
  calls$replaceClause  <- list()
  list(
    calls = calls,
    removeSubQuery = function(query, target) {
      calls$removeSubQuery <- c(
        calls$removeSubQuery,
        list(list(query = query, target = target))
      )
      query  # echo back; tests verify the forward-call, not the algorithm
    },
    replaceClause = function(query, target, replacement) {
      calls$replaceClause <- c(
        calls$replaceClause,
        list(list(query = query, target = target, replacement = replacement))
      )
      replacement
    }
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
    createSubQuery = function(keys, type, ...) {
      list(kind = "clause", keys = keys, type = type, extra = list(...))
    },
    buildQuery = function(clauses, operator) {
      list(kind = "group", clauses = clauses, operator = operator)
    },

    # Enums exposed as named lists so to_py_enum() and tests can look them up
    ClauseType = list(
      FILTER = "FILTER", SELECT = "SELECT",
      REQUIRE = "REQUIRE", ANYRECORD = "ANYRECORD"
    ),
    GroupOperator = list(AND = "AND", OR = "OR"),
    QueryType = list(
      COUNT = "count", PARTICIPANT = "participant",
      TIMESTAMP = "timestamp", CROSS_COUNT = "cross_count"
    ),
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
