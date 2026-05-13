# List available PIC-SURE platforms.

Returns the character vector of platform labels the Python \`picsure\`
package recognizes. Pass one of these strings — or a full URL to a
custom PIC-SURE deployment — as the \`platform\` argument to
\[\`connect()\`\]\[picsure::connect\].

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
