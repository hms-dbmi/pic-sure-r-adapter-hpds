test_that("buildClause() delegates to picsure_py$buildClause() with type resolved via to_py_enum", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::buildClause("\\phs1\\pht1\\phv1\\sex\\", type = "FILTER",
                                  categories = list("male"))

  expect_equal(clause$kind, "clause")
  expect_equal(clause$keys, "\\phs1\\pht1\\phv1\\sex\\")
  expect_equal(clause$type, "FILTER")  # fake's PhenotypicFilterType$FILTER value
  expect_equal(clause$extra$categories, list("male"))
})

test_that("buildClause() accepts case-insensitive type strings", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  c1 <- picsure::buildClause("x", type = "filter")
  c2 <- picsure::buildClause("x", type = "Filter")
  c3 <- picsure::buildClause("x", type = "FILTER")

  expect_equal(c1$type, "FILTER")
  expect_equal(c2$type, "FILTER")
  expect_equal(c3$type, "FILTER")
})

test_that("buildClause() errors on unknown type string", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::buildClause("x", type = "BOGUS"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(err$message, "BOGUS", fixed = TRUE)
  expect_match(err$message, "FILTER", fixed = TRUE)
})

test_that("buildClause() coerces min and max to Python ints when integral", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::buildClause("x", type = "FILTER", min = 40L, max = 80L)

  expect_identical(clause$extra$min, 40L)
  expect_identical(clause$extra$max, 80L)
})

test_that("buildClause() drops NULL optional args so Python defaults fire", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::buildClause("x", type = "FILTER")

  expect_false("min" %in% names(clause$extra))
  expect_false("max" %in% names(clause$extra))
  expect_false("categories" %in% names(clause$extra))
})

test_that("buildClause() errors on missing keys", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::buildClause(type = "FILTER"), "keys")
})

test_that("buildClause() errors on missing type", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::buildClause("x"), "type")
})

test_that("buildClause() re-raises Python exceptions as picsureError", {
  failing <- fake_picsure_py()
  failing$buildClause <- function(path, type, ...) {
    stop(structure(
      list(message = "concept path 'x' not found in dictionary"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }
  testthat::local_mocked_bindings(picsure_py = failing)

  err <- tryCatch(
    picsure::buildClause("x", type = "FILTER"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not found in dictionary", fixed = TRUE)
})

test_that("buildClauseGroup() delegates to picsure_py$buildClauseGroup()", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  c1 <- picsure::buildClause("x", type = "FILTER")
  c2 <- picsure::buildClause("y", type = "FILTER")

  group <- picsure::buildClauseGroup(list(c1, c2), operator = "AND")

  expect_equal(group$kind, "group")
  expect_length(group$clauses, 2L)
  expect_equal(group$operator, "AND")
})

test_that("buildClauseGroup() defaults operator to AND", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  c1 <- picsure::buildClause("x", type = "FILTER")

  group <- picsure::buildClauseGroup(list(c1))

  expect_equal(group$operator, "AND")
})

test_that("buildClauseGroup() accepts case-insensitive operator", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  c1 <- picsure::buildClause("x", type = "FILTER")

  g_or  <- picsure::buildClauseGroup(list(c1), operator = "or")
  g_and <- picsure::buildClauseGroup(list(c1), operator = "And")

  expect_equal(g_or$operator,  "OR")
  expect_equal(g_and$operator, "AND")
})

test_that("buildClauseGroup() errors on unknown operator string", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  c1 <- picsure::buildClause("x", type = "FILTER")

  err <- tryCatch(
    picsure::buildClauseGroup(list(c1), operator = "XOR"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(err$message, "XOR", fixed = TRUE)
  expect_match(err$message, "AND", fixed = TRUE)
})

test_that("buildClauseGroup() errors on empty clauses list", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::buildClauseGroup(list()), "clauses")
})

test_that("buildClauseGroup() accepts nested groups", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  c1 <- picsure::buildClause("x", type = "FILTER")
  c2 <- picsure::buildClause("y", type = "FILTER")
  inner <- picsure::buildClauseGroup(list(c1, c2), operator = "OR")
  c3 <- picsure::buildClause("z", type = "FILTER")

  outer <- picsure::buildClauseGroup(list(inner, c3), operator = "AND")

  expect_equal(outer$kind, "group")
  expect_length(outer$clauses, 2L)
  expect_equal(outer$clauses[[1]]$kind, "group")
  expect_equal(outer$clauses[[1]]$operator, "OR")
})

test_that("buildClauseGroup() re-raises Python exceptions as picsureError", {
  failing <- fake_picsure_py()
  failing$buildClauseGroup <- function(clauses, operator) {
    stop(structure(
      list(message = "clause tree exceeds max depth"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }
  testthat::local_mocked_bindings(picsure_py = failing)
  c1 <- list(kind = "clause", path = "x", type = "FILTER", extra = list())

  err <- tryCatch(
    picsure::buildClauseGroup(list(c1)),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "max depth", fixed = TRUE)
})

test_that("buildClause() accepts a PhenotypicFilterType member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::buildClause("\\path\\", type = picsure::PhenotypicFilterType$FILTER,
                                  categories = list("male"))

  expect_equal(clause$kind, "clause")
  expect_equal(clause$type, "FILTER")  # fake's PhenotypicFilterType$FILTER value
})

test_that("buildClause() rejects a wrong-subclass member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(
    picsure::buildClause("\\x\\", type = picsure::GroupOperator$AND),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "PhenotypicFilterType", fixed = TRUE)
})

test_that("buildClauseGroup() accepts a GroupOperator member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  group <- picsure::buildClauseGroup(
    list(list(kind = "clause")),
    operator = picsure::GroupOperator$OR
  )
  expect_equal(group$kind, "group")
  expect_equal(group$operator, "OR")  # fake's GroupOperator$OR value
})

test_that("buildClauseGroup() rejects a wrong-subclass member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(
    picsure::buildClauseGroup(
      list(list(kind = "clause")),
      operator = picsure::PhenotypicFilterType$FILTER
    ),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "GroupOperator", fixed = TRUE)
})

test_that("buildQuery() delegates to picsure_py$buildQuery() with filter and concepts", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  males <- picsure::buildClause("\\sex\\", type = "FILTER", categories = "male")
  q <- picsure::buildQuery(
    phenotypicFilter = males,
    includeConcepts = c("\\bmi\\", "\\hdl\\")
  )

  expect_equal(q$kind, "query")
  expect_equal(q$phenotypicFilter$kind, "clause")
  expect_equal(q$includeConcepts, c("\\bmi\\", "\\hdl\\"))
})

test_that("buildQuery() supports an include-only query (no filter)", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  q <- picsure::buildQuery(includeConcepts = c("\\bmi\\"))

  expect_equal(q$kind, "query")
  expect_null(q$phenotypicFilter)
  expect_equal(q$includeConcepts, "\\bmi\\")
})

test_that("buildQuery() rejects a non-character includeConcepts", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(
    picsure::buildQuery(includeConcepts = list(1, 2)),
    "includeConcepts"
  )
})

test_that("buildQuery() re-raises Python exceptions as picsureError", {
  failing <- fake_picsure_py()
  failing$buildQuery <- function(phenotypicFilter = NULL, includeConcepts = NULL) {
    stop(structure(
      list(message = "buildQuery requires a phenotypicFilter, includeConcepts, or both."),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }
  testthat::local_mocked_bindings(picsure_py = failing)

  err <- tryCatch(picsure::buildQuery(), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "phenotypicFilter", fixed = TRUE)
})
