test_that("runQuery() with type='count' returns a non-negative integer", {
  skip_unless_integration()
  session <- live_session()

  path <- Sys.getenv("PICSURE_TEST_REQUIRE_PATH", unset = NA_character_)
  if (is.na(path) || !nzchar(path)) {
    testthat::skip("PICSURE_TEST_REQUIRE_PATH not set.")
  }

  clause <- picsure::createClause(path, type = "REQUIRE")
  query  <- picsure::buildClauseGroup(list(clause), root = "AND")

  count <- picsure::runQuery(session, query, type = "count")
  expect_type(count, "integer")
  expect_gte(count, 0L)
})
