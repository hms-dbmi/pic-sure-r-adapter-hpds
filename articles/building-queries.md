# Building queries

Queries are trees of clauses combined under AND/OR operators. Build
leaves with
[`createClause()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/createClause.md),
combine them with
[`buildClauseGroup()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildClauseGroup.md),
nest groups to express arbitrary Boolean logic.

## Clause types

``` r

library(picsure)

# Categorical FILTER
sex_filter <- picsure::createClause(
  "\\phs000001\\pht000001\\phv00000001\\sex\\",
  type       = "FILTER",
  categories = list("male")
)

# Continuous FILTER
age_filter <- picsure::createClause(
  "\\phs000001\\pht000001\\phv00000005\\age\\",
  type = "FILTER", min = 40
)

# SELECT
bmi_select <- picsure::createClause(
  "\\phs000001\\pht000001\\phv00000020\\bmi\\",
  type = "SELECT"
)

# REQUIRE
smoking_require <- picsure::createClause(
  "\\phs000001\\pht000001\\phv00000030\\smoking\\",
  type = "REQUIRE"
)

# ANYRECORD
visits_anyrecord <- picsure::createClause(
  "\\phs000001\\pht000002\\phv00000100\\visit_count\\",
  type = "ANYRECORD"
)
```

You can also pass an enum member instead of a string for editor
autocomplete:

``` r

# equivalent to type = "FILTER":
sex_filter <- picsure::createClause(
  "\\phs000001\\pht000001\\phv00000001\\sex\\",
  type       = picsure::ClauseType$FILTER,
  categories = list("male")
)
```

The same applies to
`buildClauseGroup(operator = picsure::GroupOperator$AND)` and
`runQuery(type = picsure::QueryType$COUNT)`. Strings remain valid for
backwards compatibility; the enum members are new in this release.

## Combine with AND

``` r

query <- picsure::buildClauseGroup(
  list(sex_filter, age_filter, bmi_select),
  operator = "AND"
)
```

## Nested groups for AND-of-ORs

A cohort of males aged 40+ who have either COPD or asthma:

``` r

copd_filter <- picsure::createClause(
  "\\phs000001\\pht000002\\phv00000050\\copd\\",
  type = "FILTER", categories = list("Yes")
)
asthma_filter <- picsure::createClause(
  "\\phs000001\\pht000002\\phv00000051\\asthma\\",
  type = "FILTER", categories = list("Yes")
)

copd_or_asthma <- picsure::buildClauseGroup(
  list(copd_filter, asthma_filter),
  operator = "OR"
)

full_query <- picsure::buildClauseGroup(
  list(sex_filter, age_filter, copd_or_asthma),
  operator = "AND"
)
```

## Case-insensitive enums

Both `type` (clause type) and `operator` (group operator) accept
case-insensitive strings: `"filter"`, `"Filter"`, `"FILTER"` are all
equivalent.
