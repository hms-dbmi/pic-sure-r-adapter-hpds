# Building queries

Queries are trees of clauses combined under AND/OR operators. Build
leaves with
[`buildClause()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildClause.md),
combine them with
[`buildClauseGroup()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildClauseGroup.md),
nest groups to express arbitrary Boolean logic, and — when you want to
choose which concept paths come back in the output — assemble the whole
thing with
[`buildQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildQuery.md).

## Clause types

``` r

library(picsure)

# Categorical FILTER
sex_filter <- picsure::buildClause(
  "\\phs000001\\pht000001\\phv00000001\\sex\\",
  type       = "FILTER",
  categories = list("male")
)

# Continuous FILTER
age_filter <- picsure::buildClause(
  "\\phs000001\\pht000001\\phv00000005\\age\\",
  type = "FILTER", min = 40
)

# REQUIRE
smoking_require <- picsure::buildClause(
  "\\phs000001\\pht000001\\phv00000030\\smoking\\",
  type = "REQUIRE"
)

# ANYRECORD
visits_anyrecord <- picsure::buildClause(
  "\\phs000001\\pht000002\\phv00000100\\visit_count\\",
  type = "ANYRECORD"
)
```

To **include a concept path in the output without filtering** (what the
old `"SELECT"` clause type did), pass it to
`buildQuery(includeConcepts = ...)` — see “Choosing output concepts”
below. Output columns are no longer a clause type.

You can also pass an enum member instead of a string for editor
autocomplete:

``` r

# equivalent to type = "FILTER":
sex_filter <- picsure::buildClause(
  "\\phs000001\\pht000001\\phv00000001\\sex\\",
  type       = picsure::PhenotypicFilterType$FILTER,
  categories = list("male")
)
```

The same applies to
`buildClauseGroup(operator = picsure::GroupOperator$AND)` and
`runQuery(type = picsure::QueryType$COUNT)`. Strings remain valid for
backwards compatibility; the enum members are new in this release.

## Combine with AND

``` r

filters <- picsure::buildClauseGroup(
  list(sex_filter, age_filter),
  operator = "AND"
)
```

## Nested groups for AND-of-ORs

A cohort of males aged 40+ who have either COPD or asthma:

``` r

copd_filter <- picsure::buildClause(
  "\\phs000001\\pht000002\\phv00000050\\copd\\",
  type = "FILTER", categories = list("Yes")
)
asthma_filter <- picsure::buildClause(
  "\\phs000001\\pht000002\\phv00000051\\asthma\\",
  type = "FILTER", categories = list("Yes")
)

copd_or_asthma <- picsure::buildClauseGroup(
  list(copd_filter, asthma_filter),
  operator = "OR"
)

filters <- picsure::buildClauseGroup(
  list(sex_filter, age_filter, copd_or_asthma),
  operator = "AND"
)
```

## Choosing output concepts

[`buildQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/buildQuery.md)
bundles a filter tree with the concept paths to return as output
columns:

``` r

full_query <- picsure::buildQuery(
  phenotypicFilter = filters,
  includeConcepts  = c("\\phs000001\\pht000001\\phv00000020\\bmi\\")
)

# Include-only — return these concepts for every matching record, no filter:
bmi_only <- picsure::buildQuery(
  includeConcepts = c("\\phs000001\\pht000001\\phv00000020\\bmi\\")
)

# Filter only — a bare clause/clause-group runs without buildQuery():
count <- picsure::runQuery(bdc, filters, type = "count")
```

## Editing an existing query

[`removeSubQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/removeSubQuery.md)
and
[`replaceClause()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/replaceClause.md)
let you edit a query tree without rebuilding it from scratch. Both
return a **new** handle — the input is not mutated. Matching is
structural: any nested clause or clause-group that is equal by value to
`target` is matched. They accept a clause/clause-group or a Query (for a
Query, the edit applies to its phenotypic filter and the included
concepts are preserved).

``` r

# Drop the copd_or_asthma sub-tree:
without_lung <- picsure::removeSubQuery(full_query, copd_or_asthma)

# Swap "Yes" for a more specific COPD answer set:
copd_specific <- picsure::buildClause(
  "\\phs000001\\pht000002\\phv00000050\\copd\\",
  type = "FILTER",
  categories = list("Yes, severe", "Yes, very severe")
)
refined <- picsure::replaceClause(full_query, copd_filter, copd_specific)
```

[`removeSubQuery()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/removeSubQuery.md)
drops `ClauseGroup`s that are emptied by a removal, so you don’t end up
with orphan operators. It errors if the removal would empty the entire
tree — build a fresh query instead in that case.

## Save a query by name

On authorized deployments, persist a query to your user profile under a
display name with
[`saveQueryByName()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/saveQueryByName.md).
It returns the PIC-SURE query ID, which you can later pass to
[`loadQueryByID()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/loadQueryByID.md)
or
[`runQueryByID()`](https://hms-dbmi.github.io/pic-sure-r-adapter-hpds/reference/runQueryByID.md):

``` r

qid <- picsure::saveQueryByName(bdc, full_query, "Cohort 2026-Q2")

# Re-point an existing name at a refined query:
qid <- picsure::saveQueryByName(bdc, refined, "Cohort 2026-Q2", overwrite = TRUE)
```

Allowed characters in `name`: letters, digits, spaces, and
`- _ \ / ? + = [ ] . ( ) : " '` (max 255 chars; validated by the Python
adapter against the backend `NamedDataset` pattern). Not supported on
open-access platforms.

## Case-insensitive enums

Both `type` (clause type) and `operator` (group operator) accept
case-insensitive strings: `"filter"`, `"Filter"`, `"FILTER"` are all
equivalent.
