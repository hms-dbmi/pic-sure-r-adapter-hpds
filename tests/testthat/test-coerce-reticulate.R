# Enum-resolution tests that cross the real reticulate boundary.
#
# A named R list cannot show the defect these cover. `names()` on an R list is
# its member list, but `names()` on a Python enum class is every attribute,
# and for an enum that mixes in `str` that includes all 47 string methods. So
# `count`, `index`, `format`, and `strip` looked like members of every
# str-mixin enum, and a member actually named `COUNT` tied with the inherited
# `str.count` under a case-insensitive comparison. Only a genuine Python enum
# shows either, which is why these tests do not use a fake.

test_that("names() on a str-mixin Python enum is not its member list", {
  skip_unless_python_module()

  frequency <- picsure_py$VariantFrequency
  attributes_seen <- names(frequency)
  members <- picsure:::.py_enum_member_names(frequency)

  expect_setequal(members, c("RARE", "COMMON", "LOW_FREQUENCY", "ULTRA_RARE", "NOVEL"))
  expect_true(all(c("count", "index", "format", "strip") %in% attributes_seen))
  expect_false(any(c("count", "index", "format", "strip") %in% members))
  expect_gt(length(attributes_seen), length(members))
})

test_that("a member whose name collides with a string method still resolves", {
  skip_unless_python_module()

  reticulate::py_run_string("
from enum import Enum


class _CollidingEnum(str, Enum):
    COUNT = 'the-count-member'
    STRIP = 'the-strip-member'
    RARE = 'Rare'
")
  colliding <- reticulate::py$`_CollidingEnum`

  attributes_seen <- names(colliding)
  expect_true(all(c("COUNT", "count", "STRIP", "strip") %in% attributes_seen))
  expect_setequal(
    picsure:::.py_enum_member_names(colliding),
    c("COUNT", "STRIP", "RARE")
  )

  expect_equal(
    to_py_enum("COUNT", colliding, "CollidingEnum", "picsure_variant_frequency"),
    "the-count-member"
  )
  expect_equal(
    to_py_enum("strip", colliding, "CollidingEnum", "picsure_variant_frequency"),
    "the-strip-member"
  )
})

test_that("attribute lookup on a str-mixin enum really does return methods", {
  skip_unless_python_module()

  frequency <- picsure_py$VariantFrequency

  for (method_name in c("count", "index", "format", "strip")) {
    resolved <- frequency[[method_name]]
    expect_s3_class(resolved, "python.builtin.object")
    expect_false(is.character(resolved), info = method_name)
  }
})

test_that("a string method name is rejected when it is not a member", {
  skip_unless_python_module()

  err <- tryCatch(
    to_py_enum("count", picsure_py$VariantFrequency, "VariantFrequency",
               "picsure_variant_frequency"),
    error = function(e) e
  )

  expect_s3_class(err, "picsureValidationError")
  expect_s3_class(err, "picsureError")
  expect_match(conditionMessage(err), "VariantFrequency", fixed = TRUE)
  expect_match(conditionMessage(err), "RARE", fixed = TRUE)
  expect_false(grepl("KeyError", conditionMessage(err), fixed = TRUE))
})

test_that("to_py_enum resolves every member of every picsure Python enum", {
  skip_unless_python_module()

  enums <- list(
    list(obj = picsure_py$QueryType,             name = "QueryType",             subclass = "picsure_query_type"),
    list(obj = picsure_py$PhenotypicFilterType,  name = "PhenotypicFilterType",  subclass = "picsure_phenotypic_filter_type"),
    list(obj = picsure_py$GroupOperator,         name = "GroupOperator",         subclass = "picsure_group_operator"),
    list(obj = picsure_py$VariantFrequency,      name = "VariantFrequency",      subclass = "picsure_variant_frequency"),
    list(obj = picsure_py$GenomicFilterKey,      name = "GenomicFilterKey",      subclass = "picsure_genomic_filter_key"),
    list(obj = picsure_py$VariantSeverity,       name = "VariantSeverity",       subclass = "picsure_variant_severity")
  )

  for (enum in enums) {
    members <- picsure:::.py_enum_member_names(enum$obj)
    expect_gt(length(members), 0L)
    for (member in members) {
      resolved <- to_py_enum(member, enum$obj, enum$name, enum$subclass)
      expect_false(is.null(resolved), info = paste(enum$name, member))
      expect_false(is.function(resolved), info = paste(enum$name, member))
      expect_equal(
        as.character(picsure:::.py_enum_member(enum$obj, member)),
        as.character(resolved),
        info = paste(enum$name, member)
      )
    }
  }
})

test_that("to_py_enum still accepts case-insensitive strings against a real enum", {
  skip_unless_python_module()

  upper <- to_py_enum("COUNT", picsure_py$QueryType, "QueryType", "picsure_query_type")
  lower <- to_py_enum("count", picsure_py$QueryType, "QueryType", "picsure_query_type")

  expect_equal(as.character(lower), as.character(upper))
})
