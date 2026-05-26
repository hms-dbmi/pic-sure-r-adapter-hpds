# Logical operators for combining clauses in a group.

Pass a member to
\[\`buildClauseGroup()\`\]\[picsure::buildClauseGroup\]'s \`operator\`
argument. Mirrors Python's \`picsure.GroupOperator\`.

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
c1 <- picsure::buildClause("\\phs1\\sex\\",
                             type = picsure::PhenotypicFilterType$FILTER,
                             categories = "male")
c2 <- picsure::buildClause("\\phs1\\copd\\",
                             type = picsure::PhenotypicFilterType$FILTER,
                             categories = "Yes")
picsure::buildClauseGroup(
  list(c1, c2),
  operator = picsure::GroupOperator$AND
)
} # }
```
