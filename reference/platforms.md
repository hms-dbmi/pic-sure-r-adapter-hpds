# List available PIC-SURE platforms.

Returns the character vector of human-readable platform labels the
Python \`picsure\` package recognizes. These labels are display-only:
\[\`connect()\`\]\[picsure::connect\] does \*\*not\*\* accept them. To
connect, pass a \[\`Platform\`\]\[picsure::Platform\] member (e.g.
\`Platform\$BDC_AUTHORIZED\`) or a full URL to a custom PIC-SURE
deployment.

## Usage

``` r
platforms()
```

## Value

A character vector.

## See also

\[\`Platform\`\]\[picsure::Platform\], the R-side enum of known
platforms with attached connection details.

## Examples

``` r
if (FALSE) { # \dontrun{
picsure::platforms()
} # }
```
