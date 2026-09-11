# Enum-resolution tests that cross the real reticulate boundary.
#
# A named R list cannot exhibit the defect these cover. `names()` on an R list
# is its member list; `names()` on a Python enum class is every attribute,
# which for an enum that mixes in `str` includes all 47 string methods. Two
# things follow, and both are real:
#
#   * `count`, `index`, `format`, and `strip` look like members of every
#     str-mixin enum, so a caller passing one of those names got a bound
#     method forwarded to Python instead of the "not one of" error.
#   * A member actually named `COUNT` ties with the inherited `str.count`
#     under the case-insensitive comparison, and the enum was then indexed
#     with a length-2 subscript.
#
# (Attribute access does find a member that shadows a mixin method — enum
# members are set as class attributes. The hazard is the non-member name and
# the tie, not the shadowing.) Only a genuine Python enum shows any of this,
# which is why these tests live here rather than against a fake.

test_that("names() on a str-mixin Python enum is not its member list", {
  skip_unless_python_module()

  frequency <- picsure_py$VariantFrequency
  attributes_seen <- names(frequency)
  members <- picsure:::.py_enum_member_names(frequency)

  expect_setequal(members, c("RARE", "COMMON", "NOVEL"))
  # The hazard, asserted rather than assumed: string methods are attributes.
  expect_true(all(c("count", "index", "format", "strip") %in% attributes_seen))
  expect_false(any(c("count", "index", "format", "strip") %in% members))
  expect_gt(length(attributes_seen), length(members))
})

test_that("a member whose name collides with a string method still resolves", {
  skip_unless_python_module()

  # The genomic enums are the obvious next use for to_py_enum(), and all three
  # mix in str. `COUNT` is the collision that matters: `names()` on such an
  # enum reports both the member `COUNT` and the inherited `str.count`, so a
  # case-insensitive comparison matched two entries and the enum was then
  # indexed with a length-2 subscript. Resolving through `__members__` sees
  # one member and no methods.
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

  # The premise of the fix, asserted rather than assumed: a name that is a
  # string method and not a member resolves, by attribute access, to the
  # bound method. That value used to be forwarded to Python as an enum.
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
      # Whatever came back has to be the member, never a callable.
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
