# connect() tests that cross the real reticulate boundary.
#
# connect() is the one wrapper where the pinned Python build and an R-side
# whitelist have to agree, and a fake cannot make them agree.
# `fake_picsure_py()` accepts whatever kwargs it is handed, and its
# `Platform` is a named character vector of labels rather than an enum, so
# `CONNECT_EXTRA_KWARGS` can name an argument the pinned `picsure.connect()`
# has never had and every test still passes. That is how the documentation
# came to promise `timeout` and `validate` on a build that rejects both.
#
# The two tests below read the pinned build itself: the real signature of
# `picsure.connect`, and the real `Platform` enum member that
# `picsure_py$Platform[[name]]` resolves to, which is the conversion the fake
# answers with a label string. Neither test opens a network connection.

# Parameter names of the pinned build's `picsure.connect`, read through
# `inspect`. A signature's `parameters` mapping is a `mappingproxy`, a type
# reticulate has no converter for, so the names are listed Python-side and
# only the resulting list crosses the boundary.
connect_parameter_names <- function() {
  reticulate::py_run_string(
    "
import inspect
import picsure


def _picsure_connect_parameters():
    return [name for name in inspect.signature(picsure.connect).parameters]
"
  )
  reticulate::py$`_picsure_connect_parameters`()
}

test_that("every whitelisted connect() extra is accepted by the pinned build", {
  skip_unless_python_module()

  accepted <- connect_parameter_names()
  expect_true("platform" %in% accepted)

  unsupported <- setdiff(picsure:::CONNECT_EXTRA_KWARGS, accepted)
  expect_equal(
    unsupported, character(0),
    info = paste0(
      "CONNECT_EXTRA_KWARGS forwards ", paste(unsupported, collapse = ", "),
      ", which the pinned picsure.connect() does not accept. The whitelist ",
      "is what connect() prints as \"Valid extras\", so a name in it that ",
      "Python refuses is worse than no whitelist at all: the call proceeds, ",
      "Python raises an unexpected-keyword TypeError, and the researcher ",
      "gets a bare picsureError with a Python-shaped message instead of a ",
      "picsureValidationError raised before anything left R. Drop the name ",
      "from CONNECT_EXTRA_KWARGS and describe it in connect()'s @param block ",
      "as not accepted until the pin moves."
    )
  )
})

test_that("the connect() extras pending a pin bump are listed exactly when the pin allows", {
  skip_unless_python_module()

  pending_pin_bump <- character()
  accepted <- connect_parameter_names()

  listed_early <- intersect(setdiff(pending_pin_bump, accepted),
                            picsure:::CONNECT_EXTRA_KWARGS)
  expect_equal(
    listed_early, character(0),
    info = paste0(
      "CONNECT_EXTRA_KWARGS lists ", paste(listed_early, collapse = ", "),
      ", which the pinned picsure.connect() still does not accept. A key ",
      "the pinned build rejects must stay out of the whitelist so connect() ",
      "refuses it in R with a picsureValidationError naming it, rather than ",
      "forwarding it and letting Python raise an unexpected-keyword error. ",
      "Remove it until the pin moves."
    )
  )

  missing_now <- setdiff(intersect(pending_pin_bump, accepted),
                         picsure:::CONNECT_EXTRA_KWARGS)
  expect_equal(
    missing_now, character(0),
    info = paste0(
      "The pinned picsure.connect() now accepts ",
      paste(missing_now, collapse = ", "),
      ", which this package still refuses. Add the name back to ",
      "CONNECT_EXTRA_KWARGS in R/connect.R, move its description out of the ",
      "\"not accepted yet\" paragraph and into the Supported keys list in ",
      "connect()'s @param block, update the matching NEWS.md bullet, and ",
      "add it to the forwarding loop in test-connect.R."
    )
  )
})

test_that("a Platform member name resolves to a real Python Platform enum member", {
  skip_unless_python_module()

  member <- picsure::Platform$BDC_AUTHORIZED
  resolved <- picsure:::picsure_py$Platform[[member$name]]

  expect_s3_class(resolved, "enum.Enum")
  expect_s3_class(resolved, "python.builtin.object")
  expect_equal(resolved$name, member$name)
  expect_equal(resolved$value$label, member$label)
  expect_equal(resolved$value$url, member$url)
  expect_equal(resolved$value$include_consents, member$include_consents)
  expect_equal(resolved$value$requires_auth, member$requires_auth)
  expect_equal(resolved$value$supports_genomic, member$supports_genomic)
})

test_that("a Platform member the pinned enum lacks is refused as a picsureError", {
  skip_unless_python_module()

  unknown <- picsure::Platform$BDC_OPEN
  unknown$name <- "NO_SUCH_MEMBER"

  err <- tryCatch(
    picsure::connect(platform = unknown, token = "tok"),
    error = function(e) e
  )

  expect_s3_class(err, "picsureValidationError")
  expect_s3_class(err, "picsureError")
  expect_false(inherits(err, "python.builtin.KeyError"))
  expect_match(conditionMessage(err), "NO_SUCH_MEMBER", fixed = TRUE)
})
