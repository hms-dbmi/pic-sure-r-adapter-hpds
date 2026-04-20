test_that("picsure_error() creates a condition of the right classes", {
  e <- picsure_error("something bad happened")
  expect_s3_class(e, "picsureError")
  expect_s3_class(e, "error")
  expect_s3_class(e, "condition")
  expect_equal(conditionMessage(e), "something bad happened")
  expect_null(e$py_cause)
})

test_that("picsure_error() attaches py_cause when given one", {
  cause <- simpleError("python side")
  e <- picsure_error("r side message", py_cause = cause)
  expect_identical(e$py_cause, cause)
})

test_that("picsure_error() stops cleanly when raised with stop()", {
  err <- tryCatch(stop(picsure_error("boom")), error = function(e) e)
  expect_s3_class(err, "picsureError")
  expect_equal(conditionMessage(err), "boom")
})
