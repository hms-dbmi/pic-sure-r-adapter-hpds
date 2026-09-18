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

test_that("every whitelisted connect() extra is accepted by the pinned build or pending a pin bump", {
  skip_unless_python_module()

  pending_pin_bump <- c("timeout", "validate")
  accepted <- connect_parameter_names()
  expect_true("platform" %in% accepted)

  unsupported <- setdiff(picsure:::CONNECT_EXTRA_KWARGS, c(accepted, pending_pin_bump))
  expect_equal(
    unsupported, character(0),
    info = paste0(
      "CONNECT_EXTRA_KWARGS forwards ", paste(unsupported, collapse = ", "),
      ", which the pinned picsure.connect() does not accept and which this ",
      "test does not list as pending a pin bump. Passing one of these raises ",
      "a bare picsureError about an unexpected keyword argument. Either drop ",
      "the name from CONNECT_EXTRA_KWARGS, or document it in R/connect.R as ",
      "unavailable until the pin moves and add it to `pending_pin_bump` here."
    )
  )

  landed <- intersect(pending_pin_bump, accepted)
  expect_equal(
    landed, character(0),
    info = paste0(
      "The pinned picsure.connect() now accepts ", paste(landed, collapse = ", "),
      ", which this package still documents as rejected. Drop the name from ",
      "`pending_pin_bump` here and rewrite its entry in connect()'s @param ",
      "block to describe what the argument does rather than that it is ",
      "unavailable."
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
