# Create a single query clause.

Builds an opaque Clause handle suitable for nesting inside a
\[\`buildQuery()\`\]\[picsure::buildQuery\] tree and running via
\[\`runQuery()\`\]\[picsure::runQuery\].

## Usage

``` r
createSubQuery(keys, type, min = NULL, max = NULL, categories = NULL, ...)
```

## Arguments

- keys:

  One or more HPDS concept paths the clause applies to. A character
  scalar or character vector, e.g.
  \`"\phs000001\pht000001\phv00000001\sex\\\` or \`c("\path\a\\,
  "\path\b\\)\`.

- type:

  Clause type. A case-insensitive string (one of \`"FILTER"\`,
  \`"SELECT"\`, \`"REQUIRE"\`, \`"ANYRECORD"\`) or a \`ClauseType\`
  member (e.g. \[\`ClauseType\$FILTER\`\]\[picsure::ClauseType\]).

- min, max:

  Optional numeric bounds for continuous FILTER clauses.

- categories:

  Optional vector or list of accepted category values for categorical
  FILTER clauses.

- ...:

  Additional keyword arguments forwarded to the Python
  \`picsure.createSubQuery()\` call.

## Value

An opaque Clause handle.

## Examples

``` r
if (FALSE) { # \dontrun{
sex <- picsure::createSubQuery(
  "\\phs1\\pht1\\phv1\\sex\\",
  type = "FILTER", categories = "male"
)
} # }
```
