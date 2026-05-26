# Return a copy of a query with one sub-query swapped for another.

Matching is structural (see
\[\`removeSubQuery()\`\]\[picsure::removeSubQuery\]).

## Usage

``` r
replaceClause(query, target, replacement)
```

## Arguments

- query:

  A clause or clause-group handle (the query to edit).

- target:

  A clause or clause-group handle (the thing to replace).

- replacement:

  A clause or clause-group handle (the substitute).

## Value

A new clause or clause-group handle.

## Examples

``` r
if (FALSE) { # \dontrun{
adjusted <- picsure::replaceClause(full_query, old_age, new_age)
} # }
```
