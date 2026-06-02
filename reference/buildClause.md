# Create a single query clause.

Builds an opaque Clause handle suitable for nesting inside a
\[\`buildClauseGroup()\`\]\[picsure::buildClauseGroup\] tree, assembling
into a query with \[\`buildQuery()\`\]\[picsure::buildQuery\], or
running via \[\`runQuery()\`\]\[picsure::runQuery\].

## Usage

``` r
buildClause(keys, type, min = NULL, max = NULL, categories = NULL, ...)
```

## Arguments

- keys:

  One or more HPDS concept paths the clause applies to. A character
  scalar or character vector, e.g.
  \`"\phs000001\pht000001\phv00000001\sex\\\` or \`c("\path\a\\,
  "\path\b\\)\`.

- type:

  Clause type. A case-insensitive string (one of \`"FILTER"\`,
  \`"REQUIRE"\`, \`"ANYRECORD"\`) or a \`PhenotypicFilterType\` member
  (e.g.
  \[\`PhenotypicFilterType\$FILTER\`\]\[picsure::PhenotypicFilterType\]).

- min, max:

  Optional numeric bounds for continuous FILTER clauses.

- categories:

  Optional vector or list of accepted category values for categorical
  FILTER clauses.

- ...:

  Additional keyword arguments forwarded to the Python
  \`picsure.buildClause()\` call.

## Value

An opaque Clause handle.

## Details

Variables you filter on are returned as output columns automatically. To
include \*additional\* concept paths in query output without filtering,
pass them to \[\`buildQuery()\`\]\[picsure::buildQuery\]'s
\`includeConcepts\` argument — output columns are not a clause type.

## Examples

``` r
if (FALSE) { # \dontrun{
sex <- picsure::buildClause(
  "\\phs1\\pht1\\phv1\\sex\\",
  type = "FILTER", categories = "male"
)
} # }
```
