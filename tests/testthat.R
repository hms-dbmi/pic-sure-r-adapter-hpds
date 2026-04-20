library(testthat)
library(picsure)

# Unit tier runs unconditionally; integration tier runs only when enabled.
test_check("picsure")

if (identical(Sys.getenv("PICSURE_INTEGRATION"), "1")) {
  testthat::test_dir(
    file.path("testthat", "integration"),
    env      = asNamespace("picsure"),
    reporter = testthat::default_reporter()
  )
}
