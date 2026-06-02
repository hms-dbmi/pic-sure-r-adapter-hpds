# Assemble a complete query from a filter tree and/or output concepts.

Bundles a phenotypic filter (a clause or clause-group handle) with the
concept paths to include as output columns into a single Query handle
that can be passed to \[\`runQuery()\`\]\[picsure::runQuery\],
\[\`exportAsPFB()\`\]\[picsure::exportAsPFB\], or
\[\`saveQueryByName()\`\]\[picsure::saveQueryByName\].

## Usage

``` r
buildQuery(phenotypicFilter = NULL, includeConcepts = NULL)
```

## Arguments

- phenotypicFilter:

  A clause or clause-group handle (from
  \[\`buildClause()\`\]\[picsure::buildClause\] /
  \[\`buildClauseGroup()\`\]\[picsure::buildClauseGroup\]) to filter on,
  or \`NULL\` for an include-only query.

- includeConcepts:

  Optional character vector of \*additional\* concept paths to include
  as output columns, beyond the variables already named in
  \`phenotypicFilter\` (those are returned automatically). Order is
  preserved and duplicates are dropped.

## Value

An opaque Query handle.

## Examples

``` r
if (FALSE) { # \dontrun{
males <- picsure::buildClause("\\phs1\\sex\\", type = "FILTER", categories = "male")
q <- picsure::buildQuery(
  phenotypicFilter = males,
  includeConcepts = c("\\phs1\\bmi\\", "\\phs1\\hdl\\")
)
} # }
```
