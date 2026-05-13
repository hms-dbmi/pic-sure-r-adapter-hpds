# Combine clauses (and nested groups) under an AND or OR operator.

Takes a list of clause / group handles and returns a single opaque
ClauseGroup that can itself be nested inside another \`buildQuery()\`
call, or passed to \[\`runQuery()\`\]\[picsure::runQuery\].

## Usage

``` r
buildQuery(clauses, operator = "AND")
```

## Arguments

- clauses:

  A non-empty list of clause or clause-group handles.

- operator:

  The group operator. A case-insensitive string (\`"AND"\` or \`"OR"\`)
  or a \`GroupOperator\` member (e.g.
  \[\`GroupOperator\$AND\`\]\[picsure::GroupOperator\]). Defaults to
  \`"AND"\`.

## Value

An opaque ClauseGroup handle.

## Examples

``` r
if (FALSE) { # \dontrun{
sex    <- picsure::createSubQuery("\\phs1\\pht1\\phv1\\sex\\", type = "FILTER", categories = "male")
copd   <- picsure::createSubQuery("\\phs1\\pht2\\phv2\\copd\\", type = "FILTER", categories = "Yes")
asthma <- picsure::createSubQuery("\\phs1\\pht2\\phv3\\asth\\", type = "FILTER", categories = "Yes")
lung <- picsure::buildQuery(list(copd, asthma), operator = "OR")
full <- picsure::buildQuery(list(sex, lung), operator = "AND")
} # }
```
