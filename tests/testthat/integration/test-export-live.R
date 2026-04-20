test_that("exportCSV() writes a non-empty file on a live backend", {
  skip_unless_integration()
  session <- live_session()

  path <- Sys.getenv("PICSURE_TEST_REQUIRE_PATH", unset = NA_character_)
  if (is.na(path) || !nzchar(path)) {
    testthat::skip("PICSURE_TEST_REQUIRE_PATH not set.")
  }

  clause <- picsure::createClause(path, type = "SELECT")
  query  <- picsure::buildClauseGroup(list(clause), root = "AND")
  out    <- tempfile(fileext = ".csv")

  picsure::exportCSV(session, query, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0L)
})
