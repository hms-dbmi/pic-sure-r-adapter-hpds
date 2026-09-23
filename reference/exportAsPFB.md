# Export query results to a PFB file.

Runs the query and writes the result as a PFB (Portable Format for
Bioinformatics) file at \`path\`. The server builds the PFB file as a
job, and the Python adapter streams it to disk unchanged, so no PFB
library is needed on either side. The wait for the job is bounded by
\`connect()\`'s \`timeout\`.

## Usage

``` r
exportAsPFB(session, query, path)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- query:

  A clause group from \[\`buildQuery()\`\]\[picsure::buildQuery\].

- path:

  Destination file path.

## Value

The path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::exportAsPFB(bdc, full_query, "~/cohort.pfb")
} # }
```
