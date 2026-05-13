# Write a participant data frame to a CSV file.

Unlike \[\`exportAsPFB()\`\]\[picsure::exportAsPFB\], \`exportCSV\` and
\`exportTSV\` write an already-materialized data frame — they do not
re-run a query. The typical flow is \`runQuery(..., type =
"participant")\` followed by \`exportCSV(session, df, path)\`.

## Usage

``` r
exportCSV(session, data, path)
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
picsure::exportCSV(bdc, df, "~/cohort.csv")
} # }
```
