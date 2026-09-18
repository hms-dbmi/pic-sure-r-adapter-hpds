# Option-scoping helper for the tests.
#
# Sets the options, evaluates `code` with them in force, and restores whatever
# was there before, including "not set at all", which `options()` round-trips
# as NULL. `code` is a promise, forced when this function returns its value,
# so it is evaluated while the options are still in force and before
# `local_options()`'s exit handler restores them.
#
# `withr` is not an extra install. testthat already imports it, so any
# environment that can run these tests already has it.
with_picsure_options <- function(new, code) {
  withr::local_options(new)
  code
}
