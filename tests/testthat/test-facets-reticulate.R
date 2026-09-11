# Facet tests that cross the real reticulate boundary.
#
# The fake FacetSet in helper-mocks.R is an R list of closures, so it answers
# to whatever member a wrapper happens to call. That is how removeFacet()
# shipped calling `FacetSet.remove()`, a method the Python class has never
# defined: the fake grew the member, the tests passed, every real call raised
# AttributeError. These tests build a genuine picsure FacetSet inside a Python
# interpreter, so a wrapper that reaches for a member Python does not have
# fails here.

# Delegates to the shared probe in helper-python.R, which forces resolution
# rather than trusting the `delay_load = TRUE` module proxy and memoizes the
# answer so a missing interpreter is diagnosed once, not once per test.
skip_unless_python_facets <- function() {
  skip_unless_python_module("picsure._models.facet")
}

real_facet_set <- function() {
  facet <- reticulate::import("picsure._models.facet")
  facet$FacetSet(list(
    facet$FacetCategory$from_dict(list(
      name = "study_ids",
      display = "Studies",
      facets = list(
        list(name = "phs000007", display = "FHS", count = 3L),
        list(name = "phs000200", display = "WHI", count = 5L)
      )
    )),
    facet$FacetCategory$from_dict(list(
      name = "data_source",
      display = "Data Source",
      facets = list(list(name = "topmed", display = "TOPMed", count = 7L))
    ))
  ))
}

selected_values <- function(facet_set, category) {
  as.character(unlist(facet_set$view()[[category]], use.names = FALSE))
}

test_that("the Python FacetSet provides every member the R wrappers call", {
  skip_unless_python_facets()
  fs <- real_facet_set()

  for (member in c("add", "view", "clear")) {
    expect_true(reticulate::py_has_attr(fs, member), info = member)
  }
})

test_that("removeFacet() drops a value from a real Python FacetSet", {
  skip_unless_python_facets()
  fs <- real_facet_set()
  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200"))

  result <- picsure::removeFacet(fs, "study_ids", "phs000007")

  expect_identical(result, fs)
  expect_equal(selected_values(fs, "study_ids"), "phs000200")
})

test_that("removeFacet() spares other categories and clears the request body", {
  skip_unless_python_facets()
  fs <- real_facet_set()
  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200"))
  picsure::addFacet(fs, "data_source", "topmed")

  picsure::removeFacet(fs, "study_ids", "phs000007")

  expect_equal(selected_values(fs, "data_source"), "topmed")
  sent <- vapply(fs$to_request_facets(), function(f) f$name, character(1))
  expect_setequal(sent, c("phs000200", "topmed"))
})

test_that("removeFacet() removes every occurrence of a repeated value", {
  skip_unless_python_facets()
  fs <- real_facet_set()
  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200", "phs000007"))

  picsure::removeFacet(fs, "study_ids", "phs000007")

  expect_equal(selected_values(fs, "study_ids"), "phs000200")
})

test_that("removeFacet() leaves a real FacetSet untouched for an absent value", {
  skip_unless_python_facets()
  fs <- real_facet_set()
  picsure::addFacet(fs, "study_ids", c("phs000007", "phs000200"))

  picsure::removeFacet(fs, "study_ids", "phs999999")

  expect_equal(selected_values(fs, "study_ids"), c("phs000007", "phs000200"))
})

test_that("removeFacet() surfaces an unknown category as a picsureError", {
  skip_unless_python_facets()
  fs <- real_facet_set()

  err <- tryCatch(
    picsure::removeFacet(fs, "not_a_category", "phs000007"),
    error = function(e) e
  )

  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "not a valid facet category", fixed = TRUE)
})
