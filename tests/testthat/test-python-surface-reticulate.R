# The Python method names the R wrappers call, read back off the real module.
#
# Every wrapper crosses the boundary through a hard-coded attribute name, and
# helper-mocks.R writes each of those names out a second time by hand. The
# unit tier therefore proves only that the wrapper and the fake agree with
# each other. Rename `Session.loadQueryByID` in a pin bump and the fake still
# answers to the old name, so the whole unit run stays green, the parity,
# connect-signature, errors and schema guards all pass because none of them
# looks at the session, and the first real call raises an AttributeError that
# `with_picsure_error()` converts into a clean picsureError reading like a
# backend fault rather than a rename. Only the integration tier would have
# caught it, and that one needs VPN, a token and a live deployment.
#
# These assertions introspect the classes and never construct a session or
# open a connection, so they need no network and run in an ordinary
# `devtools::test()`.

# Methods the R wrappers call on a connected session object, named by the
# file that calls each one.
WRAPPED_SESSION_METHODS <- c(
  "runQuery", "loadQueryByID", "runQueryByID", "saveQueryByName",
  "facets",
  "exportAsPFB", "exportCSV", "exportTSV",
  "searchDictionary", "searchGenomicValues"
)

# Module-level callables the R wrappers reach for on the `picsure_py` handle.
WRAPPED_MODULE_CALLABLES <- c(
  "buildClause", "buildClauseGroup", "buildGenomicFilter", "buildQuery",
  "removeSubQuery", "replaceClause"
)

test_that("the Python Session provides every method the R wrappers call", {
  skip_unless_python_module("picsure")
  session_class <- reticulate::import("picsure")$Session

  for (method in WRAPPED_SESSION_METHODS) {
    expect_true(reticulate::py_has_attr(session_class, method), info = method)
  }
})

test_that("the Python picsure module provides every callable the R wrappers call", {
  skip_unless_python_module("picsure")
  module <- reticulate::import("picsure")

  for (callable in WRAPPED_MODULE_CALLABLES) {
    expect_true(reticulate::py_has_attr(module, callable), info = callable)
  }
})
