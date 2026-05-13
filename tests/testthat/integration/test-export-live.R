test_that("exportCSV() writes a non-empty file from a participant DataFrame", {
  skip_unless_integration()
  session <- live_session()

  path <- Sys.getenv("PICSURE_TEST_REQUIRE_PATH", unset = NA_character_)
  if (is.na(path) || !nzchar(path)) {
    testthat::skip("PICSURE_TEST_REQUIRE_PATH not set.")
  }

  clause <- picsure::createSubQuery(path, type = "SELECT")
  query  <- picsure::buildQuery(list(clause), operator = "AND")
  df     <- picsure::runQuery(session, query, type = "participant")
  out    <- tempfile(fileext = ".csv")

  picsure::exportCSV(session, df, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0L)
})

test_that("exportTSV() writes a non-empty file from a participant DataFrame", {
  skip_unless_integration()
  session <- live_session()

  path <- Sys.getenv("PICSURE_TEST_REQUIRE_PATH", unset = NA_character_)
  if (is.na(path) || !nzchar(path)) {
    testthat::skip("PICSURE_TEST_REQUIRE_PATH not set.")
  }

  clause <- picsure::createSubQuery(path, type = "SELECT")
  query  <- picsure::buildQuery(list(clause), operator = "AND")
  df     <- picsure::runQuery(session, query, type = "participant")
  out    <- tempfile(fileext = ".tsv")

  picsure::exportTSV(session, df, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0L)
})
