# Write a participant data frame to a TSV file.

See \[\`exportCSV()\`\]\[picsure::exportCSV\]; semantics are identical,
output is tab-separated.

## Usage

``` r
exportTSV(session, data, path)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- data:

  A \`data.frame\` (typically from
  \[\`runQuery()\`\]\[picsure::runQuery\] with \`type = "participant"\`
  or \`"timestamp"\`).

- path:

  Destination file path.

## Value

The path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- picsure::runQuery(bdc, full_query, type = "participant")
picsure::exportTSV(bdc, df, "~/cohort.tsv")
} # }
```
