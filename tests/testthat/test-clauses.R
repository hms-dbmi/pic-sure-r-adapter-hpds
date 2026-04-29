test_that("createClause() delegates to picsure_py$createClause() with type resolved via to_py_enum", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::createClause("\\phs1\\pht1\\phv1\\sex\\", type = "FILTER",
                                  categories = list("male"))

  expect_equal(clause$kind, "clause")
  expect_equal(clause$keys, "\\phs1\\pht1\\phv1\\sex\\")
  expect_equal(clause$type, "FILTER")  # fake's ClauseType$FILTER value
  expect_equal(clause$extra$categories, list("male"))
})

test_that("createClause() accepts case-insensitive type strings", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  c1 <- picsure::createClause("x", type = "filter")
  c2 <- picsure::createClause("x", type = "Filter")
  c3 <- picsure::createClause("x", type = "FILTER")

  expect_equal(c1$type, "FILTER")
  expect_equal(c2$type, "FILTER")
  expect_equal(c3$type, "FILTER")
})

test_that("createClause() errors on unknown type string", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  err <- tryCatch(
    picsure::createClause("x", type = "BOGUS"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(err$message, "BOGUS", fixed = TRUE)
  expect_match(err$message, "FILTER", fixed = TRUE)
})

test_that("createClause() coerces min and max to Python ints when integral", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::createClause("x", type = "FILTER", min = 40L, max = 80L)

  expect_identical(clause$extra$min, 40L)
  expect_identical(clause$extra$max, 80L)
})

test_that("createClause() drops NULL optional args so Python defaults fire", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::createClause("x", type = "FILTER")

  expect_false("min" %in% names(clause$extra))
  expect_false("max" %in% names(clause$extra))
  expect_false("categories" %in% names(clause$extra))
})

test_that("createClause() errors on missing keys", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::createClause(type = "FILTER"), "keys")
})

test_that("createClause() errors on missing type", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  expect_error(picsure::createClause("x"), "type")
})

test_that("createClause() re-raises Python exceptions as picsureError", {
  failing <- fake_picsure_py()
  failing$createClause <- function(path, type, ...) {
    stop(structure(
      list(message = "concept path 'x' not found in dictionary"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }
  testthat::local_mocked_bindings(picsure_py = failing)

  err <- tryCatch(
    picsure::createClause("x", type = "FILTER"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not found in dictionary", fixed = TRUE)
})

test_that("buildClauseGroup() delegates to picsure_py$buildClauseGroup()", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  c1 <- picsure::createClause("x", type = "FILTER")
  c2 <- picsure::createClause("y", type = "FILTER")

  group <- picsure::buildClauseGroup(list(c1, c2), root = "AND")

  expect_equal(group$kind, "group")
  expect_length(group$clauses, 2L)
  expect_equal(group$root, "AND")
})

test_that("buildClauseGroup() defaults root to AND", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  c1 <- picsure::createClause("x", type = "FILTER")

  group <- picsure::buildClauseGroup(list(c1))

  expect_equal(group$root, "AND")
})

test_that("buildClauseGroup() accepts case-insensitive root", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  c1 <- picsure::createClause("x", type = "FILTER")

  g_or  <- picsure::buildClauseGroup(list(c1), root = "or")
  g_and <- picsure::buildClauseGroup(list(c1), root = "And")

  expect_equal(g_or$root,  "OR")
  expect_equal(g_and$root, "AND")
})

test_that("buildClauseGroup() errors on unknown root string", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  c1 <- picsure::createClause("x", type = "FILTER")

  err <- tryCatch(
    picsure::buildClauseGroup(list(c1), root = "XOR"),
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
  c1 <- picsure::createClause("x", type = "FILTER")
  c2 <- picsure::createClause("y", type = "FILTER")
  inner <- picsure::buildClauseGroup(list(c1, c2), root = "OR")
  c3 <- picsure::createClause("z", type = "FILTER")

  outer <- picsure::buildClauseGroup(list(inner, c3), root = "AND")

  expect_equal(outer$kind, "group")
  expect_length(outer$clauses, 2L)
  expect_equal(outer$clauses[[1]]$kind, "group")
  expect_equal(outer$clauses[[1]]$root, "OR")
})

test_that("buildClauseGroup() re-raises Python exceptions as picsureError", {
  failing <- fake_picsure_py()
  failing$buildClauseGroup <- function(clauses, root) {
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

test_that("createClause() accepts a ClauseType member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  clause <- picsure::createClause("\\path\\", type = picsure::ClauseType$FILTER,
                                  categories = list("male"))

  expect_equal(clause$kind, "clause")
  expect_equal(clause$type, "FILTER")  # fake's ClauseType$FILTER value
})

test_that("createClause() rejects a wrong-subclass member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(
    picsure::createClause("\\x\\", type = picsure::GroupOperator$AND),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "ClauseType", fixed = TRUE)
})

test_that("buildClauseGroup() accepts a GroupOperator member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  group <- picsure::buildClauseGroup(
    list(list(kind = "clause")),
    root = picsure::GroupOperator$OR
  )
  expect_equal(group$kind, "group")
  expect_equal(group$root, "OR")  # fake's GroupOperator$OR value
})

test_that("buildClauseGroup() rejects a wrong-subclass member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)

  err <- tryCatch(
    picsure::buildClauseGroup(
      list(list(kind = "clause")),
      root = picsure::ClauseType$FILTER
    ),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "GroupOperator", fixed = TRUE)
})
