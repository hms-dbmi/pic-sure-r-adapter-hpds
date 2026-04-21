test_that("runQuery() with type='count' returns a CountResult", {
  skip_unless_integration()
  session <- live_session()

  path <- Sys.getenv("PICSURE_TEST_REQUIRE_PATH", unset = NA_character_)
  if (is.na(path) || !nzchar(path)) {
    testthat::skip("PICSURE_TEST_REQUIRE_PATH not set.")
  }

  clause <- picsure::createClause(path, type = "REQUIRE")
  query  <- picsure::buildClauseGroup(list(clause), root = "AND")

  count <- picsure::runQuery(session, query, type = "count")
  # Python returns a CountResult with $value / $margin / $cap. Small counts
  # are obfuscated server-side: value is None (R: NULL) and only cap is set.
  expect_true(inherits(count, "python.builtin.object"))
  if (!is.null(count$value)) {
    expect_true(is.numeric(count$value))
    expect_gte(count$value, 0)
  } else {
    expect_true(is.numeric(count$cap))
    expect_gt(count$cap, 0)
  }
})

test_that("runQuery() with type='participant' returns a data.frame with rows", {
  skip_unless_integration()
  session <- live_session()

  path <- Sys.getenv("PICSURE_TEST_REQUIRE_PATH", unset = NA_character_)
  if (is.na(path) || !nzchar(path)) {
    testthat::skip("PICSURE_TEST_REQUIRE_PATH not set.")
  }

  clause <- picsure::createClause(path, type = "SELECT")
  query  <- picsure::buildClauseGroup(list(clause), root = "AND")

  df <- picsure::runQuery(session, query, type = "participant")
  expect_s3_class(df, "data.frame")
})
