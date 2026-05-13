# Load a previously-saved PIC-SURE query by its query ID.

Fetches the saved query body from the PIC-SURE backend and rebuilds it
as a Clause or ClauseGroup that can be passed back into
\[\`runQuery()\`\]\[picsure::runQuery\],
\[\`exportAsPFB()\`\]\[picsure::exportAsPFB\], or composed inside
another \[\`buildClauseGroup()\`\]\[picsure::buildClauseGroup\].

## Usage

``` r
loadQueryByID(session, query_id)
```

## Arguments

- session:

  A session object produced by \[\`connect()\`\]\[picsure::connect\].

- query_id:

  The UUID string of a previously-saved query.

## Value

An opaque Clause / ClauseGroup handle suitable for \`runQuery()\`.

## Examples

``` r
if (FALSE) { # \dontrun{
previous <- picsure::loadQueryByID(bdc, "11111111-2222-3333-4444-555555555555")
count <- picsure::runQuery(bdc, previous, type = "count")
} # }
```
