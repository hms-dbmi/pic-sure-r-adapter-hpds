# Helpers shared by the integration tier. Loaded automatically by testthat
# because of the helper-*.R naming convention when the integration tests
# are discovered.

skip_unless_integration <- function() {
  if (!identical(Sys.getenv("PICSURE_INTEGRATION"), "1")) {
    testthat::skip("Integration tier disabled; set PICSURE_INTEGRATION=1 to enable.")
  }
}

live_platform <- function() {
  Sys.getenv("PICSURE_TEST_PLATFORM", unset = "Demo")
}

live_token <- function() {
  token <- Sys.getenv("PICSURE_TEST_TOKEN", unset = NA_character_)
  if (is.na(token) || !nzchar(token)) {
    testthat::skip("PICSURE_TEST_TOKEN not set.")
  }
  token
}

# Cache the live session across tests in a single process — connecting is
# expensive and the integration tier is read-only, so sharing is safe.
.live_session_cache <- new.env(parent = emptyenv())

live_session <- function() {
  if (is.null(.live_session_cache$session)) {
    .live_session_cache$session <- picsure::connect(
      platform = live_platform(),
      token    = live_token()
    )
  }
  .live_session_cache$session
}
