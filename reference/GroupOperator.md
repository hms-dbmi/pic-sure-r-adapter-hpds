# Logical operators for combining clauses in a group.

Pass a member to \[\`buildQuery()\`\]\[picsure::buildQuery\]'s
\`operator\` argument. Mirrors Python's \`picsure.GroupOperator\`.

## Usage

``` r
GroupOperator
```

## Format

A list of \`picsure_enum_member\` objects:

- \`AND\`:

  All clauses must match.

- \`OR\`:

  At least one clause must match.

## Examples

``` r
if (FALSE) { # \dontrun{
c1 <- picsure::createSubQuery("\\phs1\\sex\\",
                             type = picsure::ClauseType$FILTER,
                             categories = "male")
c2 <- picsure::createSubQuery("\\phs1\\copd\\",
                             type = picsure::ClauseType$FILTER,
                             categories = "Yes")
picsure::buildQuery(
  list(c1, c2),
  operator = picsure::GroupOperator$AND
)
} # }
```
