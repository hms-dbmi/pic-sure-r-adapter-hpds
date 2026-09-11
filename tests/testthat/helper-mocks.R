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
  calls$searchGenomicValues <- list()
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
          variant_count = list(value = 7L, margin = 0L, cap = NULL),
          variant_list = c("chr1:1:A:T", "chr2:2:G:C"),
          vcf_excerpt = data.frame(
            CHROM = "1", POSITION = 100L, REF = "A", ALT = "T",
            stringsAsFactors = FALSE
          ),
          aggregate_vcf_excerpt = data.frame(
            CHROM = "1", POSITION = 100L, REF = "A", ALT = "T",
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
          variant_count = list(value = 7L, margin = 0L, cap = NULL),
          variant_list = c("chr1:1:A:T", "chr2:2:G:C"),
          vcf_excerpt = data.frame(
            CHROM = "1", POSITION = 100L, REF = "A", ALT = "T",
            stringsAsFactors = FALSE
          ),
          aggregate_vcf_excerpt = data.frame(
            CHROM = "1", POSITION = 100L, REF = "A", ALT = "T",
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
      searchGenomicValues = function(...) {
        calls$searchGenomicValues <- c(calls$searchGenomicValues, list(list(...)))
        data.frame(value = c("BRCA1", "BRCA2"), stringsAsFactors = FALSE)
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

fake_picsure_py <- function() {
  calls <- new.env(parent = emptyenv())
  calls$connect <- list()

  list(
    # Module-level callables
    connect = function(platform, token, ...) {
      calls$connect <- c(calls$connect, list(list(platform = platform, token = token, ...)))
      new_fake_session(platform = platform, token = token)
    },
    buildClause = function(keys, type, ...) {
      list(kind = "clause", keys = keys, type = type, extra = list(...))
    },
    buildClauseGroup = function(clauses, operator) {
      list(kind = "group", clauses = clauses, operator = operator)
    },
    buildQuery = function(phenotypicFilter = NULL, includeConcepts = NULL, genomicFilters = NULL) {
      list(
        kind = "query",
        phenotypicFilter = phenotypicFilter,
        includeConcepts = includeConcepts,
        genomicFilters = genomicFilters
      )
    },
    # `extra` holds whatever arrived through `...`, the way the fake
    # `buildClause` does. Without it the fake silently swallowed unknown
    # kwargs, so a test could not tell "the wrapper forwarded min/max as
    # unknown kwargs" from "the wrapper absorbed them".
    buildGenomicFilter = function(key, values = NULL, ...) {
      list(kind = "genomic_filter", key = key, values = values, extra = list(...))
    },
    genomicConsequences = function() {
      data.frame(
        severity = c("High Severity", "Medium Severity"),
        consequence = c("stop_gained", "missense_variant"),
        stringsAsFactors = FALSE
      )
    },

    # Enums exposed as named lists so to_py_enum() and tests can look them up
    PhenotypicFilterType = list(
      FILTER = "FILTER",
      REQUIRE = "REQUIRE", ANYRECORD = "ANYRECORD"
    ),
    GroupOperator = list(AND = "AND", OR = "OR"),
    QueryType = list(
      COUNT = "count", PARTICIPANT = "participant",
      TIMESTAMP = "timestamp", CROSS_COUNT = "cross_count",
      VARIANT_COUNT = "variant_count", VARIANT_LIST = "variant_list",
      VCF_EXCERPT = "vcf_excerpt", AGGREGATE_VCF_EXCERPT = "aggregate_vcf_excerpt"
    ),
    VariantFrequency = list(RARE = "Rare", COMMON = "Common", NOVEL = "Novel"),
    GenomicFilterKey = list(
      GENE_WITH_VARIANT = "Gene_with_variant",
      VARIANT_CONSEQUENCE_CALCULATED = "Variant_consequence_calculated",
      VARIANT_FREQUENCY_AS_TEXT = "Variant_frequency_as_text",
      VARIANT_CLASS = "Variant_class",
      VARIANT_SEVERITY = "Variant_severity"
    ),
    VariantSeverity = list(
      HIGH = "High Severity", MEDIUM = "Medium Severity", LOW = "Low Severity"
    ),
    # `Platform` exists here only as connect()'s name -> label lookup table,
    # for `picsure_py$Platform[[member$name]]`. It is deliberately NOT a
    # stand-in for the real enum, and platforms() must never be tested
    # against it: the real `picsure.Platform` is a Python Enum class whose
    # `__members__` crosses the reticulate boundary as an unconverted
    # `mappingproxy`, and this character vector takes a different branch of
    # platforms() entirely. That is how a conversion bug that broke every
    # real call shipped green. platforms() is exercised against a genuine
    # Python enum in test-platforms-reticulate.R.
    #
    # Derived from the R-side enum so every member name resolves and the two
    # cannot drift apart.
    Platform = vapply(picsure::Platform, function(m) m$label, character(1)),

    # Call recorder
    .calls = calls
  )
}

# Fake FacetSet: a mutable Python-like object recording the calls R wrappers
# make. Its surface is exactly the Python FacetSet's, `$add(category, values)`,
# `$view()`, `$clear(category)`, and deliberately nothing more. An earlier
# version of this fake carried a `$remove()` member that Python has never had,
# which let a broken `removeFacet()` pass its tests. Only add a member here
# after checking it against `_models/facet.py` in the Python adapter.
new_fake_facet_set <- function(categories = c("study_ids", "data_source")) {
  state <- new.env(parent = emptyenv())
  state$entries <- list()  # list of list(key = ..., value = ...)

  values_for <- function(category) {
    matching <- Filter(function(e) identical(e$key, category), state$entries)
    vapply(matching, function(e) e$value, character(1))
  }

  fs <- structure(
    list(
      add = function(key, value) {
        for (v in as.character(unlist(value, use.names = FALSE))) {
          state$entries <- c(state$entries, list(list(key = key, value = v)))
        }
        invisible(NULL)
      },
      view = function() {
        seen <- unique(c(categories, vapply(state$entries, function(e) e$key, character(1))))
        stats::setNames(lapply(seen, values_for), seen)
      },
      clear = function(category = NULL) {
        if (is.null(category)) {
          state$entries <- list()
          return(invisible(NULL))
        }
        keep <- vapply(state$entries, function(e) !identical(e$key, category), logical(1))
        state$entries <- state$entries[keep]
        invisible(NULL)
      },
      .state = state
    ),
    class = "fake_facet_set"
  )
  fs
}
