# Return a copy of a query with all matches of a sub-query removed.

Matching is structural: any nested clause or clause-group that equals
\`target\` (by value) is dropped. Empty groups left behind are removed
automatically. Errors if the resulting query would be empty.

## Usage

``` r
removeSubQuery(query, target)
```

## Arguments

- query:

  A clause or clause-group handle (the query to edit).

- target:

  A clause or clause-group handle (the thing to remove).

## Value

A new clause or clause-group handle. The original \`query\` is not
mutated.

## Examples

``` r
if (FALSE) { # \dontrun{
smaller <- picsure::removeSubQuery(full_query, age_filter)
} # }
```
