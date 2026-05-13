# Export query results to a PFB file.

Runs the query and writes the result as a PFB (Portable Format for
Bioinformatics) file at \`path\`. Requires the Python \`picsure\[pfb\]\`
optional dependency on the Python side.

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
