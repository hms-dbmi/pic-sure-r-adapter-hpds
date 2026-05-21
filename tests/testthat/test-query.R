test_that("runQuery() with type='count' returns a CountResult-shaped object", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  result <- picsure::runQuery(bdc, q, type = "count")

  expect_true(!is.null(result$value))
  expect_equal(result$value, 42L)
})

test_that("runQuery() with type='participant' returns a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  result <- picsure::runQuery(bdc, q, type = "participant")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("patient_id" %in% names(result))
})

test_that("runQuery() accepts case-insensitive type", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  r1 <- picsure::runQuery(bdc, q, type = "COUNT")
  r2 <- picsure::runQuery(bdc, q, type = "Count")
  r3 <- picsure::runQuery(bdc, q, type = "count")

  expect_equal(r1$value, 42L)
  expect_equal(r2$value, 42L)
  expect_equal(r3$value, 42L)
})

test_that("runQuery() defaults type to 'count'", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  result <- picsure::runQuery(bdc, q)

  expect_equal(result$value, 42L)
})

test_that("runQuery() forwards query and type to session$runQuery", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  picsure::runQuery(bdc, q, type = "participant")

  call <- bdc$.calls$runQuery[[1]]
  expect_identical(call$query, q)
  expect_equal(tolower(call$type), "participant")
})

test_that("runQuery() errors on missing query", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  expect_error(picsure::runQuery(bdc), "query")
})

test_that("runQuery() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$runQuery <- function(...) {
    stop(structure(
      list(message = "query rejected: empty phenotypic clause"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(
    picsure::runQuery(bdc, list(kind = "group", clauses = list(), operator = "AND")),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "empty phenotypic clause", fixed = TRUE)
})

test_that("runQuery() with type='timestamp' returns a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  result <- picsure::runQuery(bdc, q, type = "timestamp")

  expect_s3_class(result, "data.frame")
  expect_true("timestamp" %in% names(result))
})

test_that("runQuery() forwards an unknown type to Python for the error", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  q <- list(kind = "group", clauses = list(), operator = "AND")

  err <- tryCatch(
    picsure::runQuery(bdc, q, type = "nonsense"),
    error = function(e) e
  )
  expect_s3_class(err, "error")
})

test_that("runQuery() accepts a QueryType member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  result <- picsure::runQuery(session, query = list(kind = "group"),
                              type = picsure::QueryType$COUNT)

  recorded <- session$.calls$runQuery[[1]]
  expect_equal(recorded$type, "count")  # fake's QueryType$COUNT value
  expect_equal(result$value, 42L)
})

test_that("runQuery() rejects a wrong-subclass member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  err <- tryCatch(
    picsure::runQuery(session, query = list(kind = "group"),
                      type = picsure::PhenotypicFilterType$FILTER),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "QueryType", fixed = TRUE)
})

test_that("runQuery() still accepts case-insensitive strings (backwards compat)", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  picsure::runQuery(session, query = list(kind = "group"), type = "PARTICIPANT")

  recorded <- session$.calls$runQuery[[1]]
  # to_py_enum's case-insensitive lookup resolves "PARTICIPANT" against
  # the fake's PARTICIPANT key and returns the fake's stored lowercase
  # value "participant". Pin the exact case so a future refactor that
  # forwards the raw input would fail this assertion.
  expect_equal(recorded$type, "participant")
})

test_that("loadQueryByID() forwards query_id to session$loadQueryByID and returns its result", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  result <- picsure::loadQueryByID(bdc, "abc-123")

  expect_equal(result$loaded_from, "abc-123")
  call <- bdc$.calls$loadQueryByID[[1]]
  expect_identical(call$query_id, "abc-123")
})

test_that("loadQueryByID() result can be piped back into runQuery()", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  loaded <- picsure::loadQueryByID(bdc, "abc-123")
  count <- picsure::runQuery(bdc, loaded, type = "count")

  expect_equal(count$value, 42L)
})

test_that("loadQueryByID() errors on missing query_id", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  expect_error(picsure::loadQueryByID(bdc), "query_id")
})

test_that("loadQueryByID() rejects empty, NA, or non-scalar query_id", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  expect_error(picsure::loadQueryByID(bdc, ""), "query_id")
  expect_error(picsure::loadQueryByID(bdc, NA_character_), "query_id")
  expect_error(picsure::loadQueryByID(bdc, c("a", "b")), "query_id")
  expect_error(picsure::loadQueryByID(bdc, 123), "query_id")
})

test_that("loadQueryByID() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$loadQueryByID <- function(query_id) {
    stop(structure(
      list(message = "query 'abc-123' not found"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(
    picsure::loadQueryByID(bdc, "abc-123"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not found", fixed = TRUE)
})

test_that("runQueryByID() forwards query_id and type to session$runQueryByID", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  result <- picsure::runQueryByID(bdc, "abc-123", type = "count")

  expect_equal(result$value, 42L)
  call <- bdc$.calls$runQueryByID[[1]]
  expect_identical(call$query_id, "abc-123")
  expect_equal(tolower(call$type), "count")
})

test_that("runQueryByID() defaults type to 'count'", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  result <- picsure::runQueryByID(bdc, "abc-123")

  expect_equal(result$value, 42L)
  call <- bdc$.calls$runQueryByID[[1]]
  expect_equal(tolower(call$type), "count")
})

test_that("runQueryByID() with type='participant' returns a data.frame", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  result <- picsure::runQueryByID(bdc, "abc-123", type = "participant")

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("runQueryByID() accepts a QueryType member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  picsure::runQueryByID(session, "abc-123", type = picsure::QueryType$COUNT)

  recorded <- session$.calls$runQueryByID[[1]]
  expect_equal(recorded$type, "count")
})

test_that("runQueryByID() rejects a wrong-subclass member", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  session <- new_fake_session()

  err <- tryCatch(
    picsure::runQueryByID(session, "abc-123", type = picsure::PhenotypicFilterType$FILTER),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(err$message, "QueryType", fixed = TRUE)
})

test_that("runQueryByID() rejects empty, NA, or non-scalar query_id", {
  testthat::local_mocked_bindings(picsure_py = fake_picsure_py())
  bdc <- picsure::connect(platform = "Demo", token = "tok")

  expect_error(picsure::runQueryByID(bdc), "query_id")
  expect_error(picsure::runQueryByID(bdc, ""), "query_id")
  expect_error(picsure::runQueryByID(bdc, NA_character_), "query_id")
  expect_error(picsure::runQueryByID(bdc, c("a", "b")), "query_id")
  expect_error(picsure::runQueryByID(bdc, 123), "query_id")
})

test_that("runQueryByID() re-raises Python exceptions as picsureError", {
  fake <- fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake)
  bdc <- picsure::connect(platform = "Demo", token = "tok")
  bdc$runQueryByID <- function(query_id, type = "count") {
    stop(structure(
      list(message = "query 'abc-123' not found"),
      class = c("python.builtin.Exception", "error", "condition")
    ))
  }

  err <- tryCatch(
    picsure::runQueryByID(bdc, "abc-123"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not found", fixed = TRUE)
})

test_that("removeSubQuery forwards to picsure_py$removeSubQuery", {
  fake <- new_fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake, .package = "picsure")

  picsure::removeSubQuery("QUERY", "TARGET")
  expect_length(fake$calls$removeSubQuery, 1L)
  expect_equal(fake$calls$removeSubQuery[[1]]$query,  "QUERY")
  expect_equal(fake$calls$removeSubQuery[[1]]$target, "TARGET")
})

test_that("removeSubQuery rejects missing args", {
  expect_error(picsure::removeSubQuery(query = "Q"), "`target` is required")
  expect_error(picsure::removeSubQuery(target = "T"), "`query` is required")
})

test_that("replaceClause forwards all three args", {
  fake <- new_fake_picsure_py()
  testthat::local_mocked_bindings(picsure_py = fake, .package = "picsure")

  picsure::replaceClause("Q", "T", "R")
  expect_length(fake$calls$replaceClause, 1L)
  expect_equal(fake$calls$replaceClause[[1]]$query,       "Q")
  expect_equal(fake$calls$replaceClause[[1]]$target,      "T")
  expect_equal(fake$calls$replaceClause[[1]]$replacement, "R")
})

test_that("saveQueryByName forwards to session and returns the query id", {
  session <- new_fake_session()
  qid <- picsure::saveQueryByName(session, "QUERY", "Cohort A")
  expect_identical(qid, "qid-fake-001")
  expect_equal(session$.calls$saveQueryByName[[1]]$overwrite, FALSE)
})

test_that("saveQueryByName forwards overwrite=TRUE", {
  session <- new_fake_session()
  picsure::saveQueryByName(session, "QUERY", "Cohort A", overwrite = TRUE)
  expect_equal(session$.calls$saveQueryByName[[1]]$overwrite, TRUE)
})

test_that("saveQueryByName validates name and overwrite", {
  session <- new_fake_session()
  expect_error(picsure::saveQueryByName(session, "Q", ""),  "non-empty")
  expect_error(picsure::saveQueryByName(session, "Q", NA_character_), "non-empty")
  expect_error(picsure::saveQueryByName(session, "Q", "Cohort", overwrite = NA),
               "single logical")
})
