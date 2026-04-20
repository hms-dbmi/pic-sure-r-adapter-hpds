test_that("connect() opens a live session", {
  skip_unless_integration()
  session <- live_session()
  expect_true(inherits(session, "python.builtin.object"))
})

test_that("connect() rejects an obviously-bad token", {
  skip_unless_integration()
  err <- tryCatch(
    picsure::connect(platform = live_platform(), token = "definitely-not-a-real-token"),
    error = function(e) e
  )
  expect_s3_class(err, "picsureError")
})
